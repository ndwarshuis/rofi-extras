{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

--------------------------------------------------------------------------------
-- | rofi-dev - a rofi prompt for mountable devices
--
-- Like all "mount helpers" this is basically a wrapper for low-level utilities
-- the mount things from the command line. It also creates/destroys mountpoint
-- paths given a specific location for such mountpoints.

module Main (main) where

import           Bitwarden.Internal

import           Control.Monad
import           Control.Monad.Reader

import           Data.Either
import           Data.List
import           Data.List.Split       (splitOn)
import qualified Data.Map              as M
import qualified Data.Map.Ordered      as O
import           Data.Maybe
import           Data.Text             (unpack)

import           Rofi.Command

import           Text.Printf
import           Text.Regex.TDFA
import           Text.Wrap

import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.Posix.User     (getEffectiveUserName)
import           System.Process

import           UnliftIO.Exception

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse args = case getOpt Permute options args of
  (o, n, [])   -> initMountConf n >>= \i -> runMounts $ foldl (flip id) i o
  (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo h options
  where
    h = "Usage: rofi-dev [OPTIONS] [-- ROFI-OPTIONS]"

options :: [OptDescr (MountConf -> MountConf)]
options =
  [ Option ['s'] ["secret"]
    (ReqArg (\s m -> m { passwords = addSecret (passwords m) s } ) "SECRET")
    $ wrap "Use libsecret to retrieve password for DIR using ATTR/VAL pairs. \
           \The pairs will be supplied to a 'secret-tool lookup' call. \
           \ Argument is formatted like 'DIR:ATTR1=VAL1,ATTR2=VAL2...'"
  , Option ['b'] ["bitwarden"]
    (ReqArg (\s m -> m { passwords = addBitwarden (passwords m) s } ) "BW")
    $ wrap "Use the Bitwarden CLI to retrieve a password for DIR. \
           \The argument is formatted like 'DIR:NAME' where NAME is the \
           \name of the Bitwarden entry to find."
  , Option ['p'] ["password"]
    (ReqArg (\s m -> m { passwords = addPwdPrompt (passwords m) s } ) "DIR")
    "Prompt for password when mounting DIR."
  , Option ['d'] ["directory"]
    (ReqArg (\s m -> m { mountDir = s } ) "DIR")
    $ wrap "The DIR in which new mountpoints will be created. This is assumed \
           \to be writable to the current user, and will be used for fuse \
           \entries as well as user mounts in fstab. For the latter, it is \
           \assumed that all user mounts contain this directory if a \
           \mountpoint does not already exist for them. If not given this will \
           \default to '/tmp/media/USER'."
  , Option ['v'] ["veracrypt"]
    (ReqArg (\s m -> m { vcMounts = addVeracryptMount (vcMounts m) s } ) "VC")
    $ wrap "A veracrypt mount specification formatted like DIR:VOL where \
           \DIR is the mountpoint and VOL is the path to the encrypted \
           \volume. To specify a password, use the -p, -b- or -s options."
  ]
  where
    wrap = unpack . wrapText defaultWrapSettings 40

--------------------------------------------------------------------------------
-- | Static configuration
--
-- This is defined by the mount options on the command line, and holds:
-- - a map between mountpoints and a means to get passwords when mounting those
--   mountpoints
-- - a mount directory where mountpoints will be created if needed (defaults
--   to '/tmp/media/USER'
-- - any arguments to be passed to the rofi command

type PasswordGetter = IO (Maybe String)

type MountpointPasswords = M.Map String PasswordGetter

type VeracryptMount = (FilePath, FilePath)

addVeracryptMount :: [VeracryptMount] -> String -> [VeracryptMount]
addVeracryptMount l s = case splitPrefix s of
  (dir, ":", vol) -> (dir, vol):l
  _               -> l

-- TODO check if mountdir exists or puke
data MountConf = MountConf
    { passwords :: MountpointPasswords
    , mountDir  :: FilePath
    , rofiArgs  :: [String]
    , vcMounts  :: [VeracryptMount]
    }

instance RofiConf MountConf where
  defArgs MountConf { rofiArgs = a } = a

initMountConf :: [String] -> IO MountConf
initMountConf a = conf <$> getEffectiveUserName
  where
    conf u = MountConf
      { passwords = M.empty
      , mountDir = "/tmp/media" </> u
      , rofiArgs = a
      , vcMounts = []
      }

--------------------------------------------------------------------------------
-- | Password-getting functions

addSecret :: MountpointPasswords -> String -> MountpointPasswords
addSecret pwds c = case splitPrefix c of
  (dir, ":", r) -> M.insert dir (runSecret $ fromCommaSepString' r) pwds
  _             -> pwds

runSecret :: [(String, String)] -> PasswordGetter
runSecret kvs = readCmdSuccess "secret-tool" ("lookup":kvs') ""
  where
    kvs' = concatMap (\(k, v) -> [k, v]) kvs

addBitwarden :: MountpointPasswords -> String -> MountpointPasswords
addBitwarden pwds c = case splitPrefix c of
  (dir, ":", name) -> M.insert dir (runBitwarden name) pwds
  _                -> pwds

runBitwarden :: String -> PasswordGetter
runBitwarden pname = ((password . login) <=< find (\i -> name i == pname))
  <$> getItems

addPwdPrompt :: MountpointPasswords -> String -> MountpointPasswords
addPwdPrompt pwds dir = M.insert dir readPassword pwds

splitPrefix :: String -> (String, String, String)
splitPrefix s = s =~ (":" :: String)

--------------------------------------------------------------------------------
-- | Main prompt
--
-- This command will have one Rofi prompt and will display all available
-- mounts grouped by device type (eg removable, sshfs, cifs, etc). I like
-- pretty things, so ensure the entries are aligned properly as well

runMounts :: MountConf -> IO ()
runMounts c = runRofiIO c $ runPrompt =<< getGroups

runPrompt :: RofiConf c => [RofiGroup c] -> RofiIO c ()
runPrompt gs = selectAction $ emptyMenu
               { groups = gs
               , prompt = Just "Select Device"
               }

getGroups :: RofiIO MountConf [RofiGroup MountConf]
getGroups = do
  fstab <- readFSTab
  sequence
    [ mkGroup "SSHFS Devices" $ sshfsDevices fstab
    , mkGroup "CIFS Devices" $ cifsDevices fstab
    , mkGroup "Veracrypt Devices" =<< getVeracryptDevices
    , mkGroup "Removable Devices" =<< getRemovableDevices
    , mkGroup "MTP Devices" =<< getMTPDevices
    ]

mkGroup :: Mountable d => String -> [d] -> RofiIO MountConf (RofiGroup MountConf)
mkGroup header devs = titledGroup header . alignEntries . toRofiActions
    <$> mapM mkAction devs

alignSep :: String
alignSep = " | "

alignSepPre :: String
alignSepPre = "@@@"

alignEntries :: RofiActions c -> RofiActions c
alignEntries = O.fromList . withKeys . O.assocs
  where
    withKeys as = let (ks, vs) = unzip as in zip (align ks) vs
    align = fmap (intercalate alignSep)
      . transpose
      . mapToLast pad
      . transpose
      . fmap (splitOn alignSepPre)
    pad xs = let m = getMax xs in fmap (\x -> take m (x ++ repeat ' ')) xs
    getMax = maximum . fmap length
    mapToLast _ []     = []
    mapToLast _ [x]    = [x]
    mapToLast f (x:xs) = f x : mapToLast f xs

--------------------------------------------------------------------------------
-- | Removable devices
--
-- A device which can be removed (which is all the devices we care about)
-- This can be minimally described by a device DEVICESPEC and LABEL.

data Removable = Removable
    { deviceSpec :: String
    , label      :: String
    }
    deriving (Eq, Show)

instance Mountable Removable where
  mount Removable { deviceSpec = d, label = l } m =
    io $ runMountNotify "udisksctl" [c, "-b", d] l m
    where
      c = if m then "unmount" else "mount"

  allInstalled _ = fmap isJust $ io $ findExecutable "udisksctl"

  isMounted Removable { deviceSpec = d } = elem d <$> io curDeviceSpecs

  fmtEntry Removable { deviceSpec = d, label = l } = l ++ alignSepPre ++ d

-- | Return list of possible rofi actions for removable devices
-- A 'removable device' is defined as a hotplugged device with a filesystem as
-- reported by 'lsblk'. If the LABEL does not exist on the filesystem, the
-- label shown on the prompt will be 'SIZE Volume' where size is the size of
-- the device
getRemovableDevices :: RofiConf c => RofiIO c [Removable]
getRemovableDevices = fromLines toDev . lines
  <$> io (readProcess "lsblk" ["-n", "-r", "-o", columns] "")
  where
    columns = "FSTYPE,HOTPLUG,PATH,LABEL,SIZE"
    -- can't use 'words' here since it will drop spaces in the front
    toDev line = case splitBy ' ' line of
      ("":_)             -> Nothing
      [_, "1", d, "", s] -> mk d $ s ++ " Volume"
      [_, "1", d, l, _]  -> mk d l
      _                  -> Nothing
    mk d l = Just $ Removable { deviceSpec = d, label = l }

--------------------------------------------------------------------------------
-- | CIFS Devices
--
-- This wraps the Removable device (since it is removable) and also adds its
-- own mount options and passwords for authentication.

data CIFS = CIFS Removable FilePath (Maybe PasswordGetter)

instance Mountable CIFS where
  mount (CIFS Removable{ label = l } m getPwd) False =
    bracketOnError_
      (mkDirMaybe m)
      (rmDirMaybe m)
      $ io $ do
      res <- case getPwd of
        Just pwd -> do
          p <- maybe [] (\p -> [("PASSWD", p)]) <$> pwd
          readCmdEither' "mount" [m] "" p
        Nothing -> readCmdEither "mount" [m] ""
      notifyMounted (isRight res) False l

  mount (CIFS Removable{ label = l } m _) True = umountNotify l m

  allInstalled _ = io $ isJust <$> findExecutable "mount.cifs"

  isMounted (CIFS _ dir _) = io $ isDirMounted dir

  fmtEntry (CIFS r _ _) = fmtEntry r

-- TODO this smells like something that should be in a typeclass
fstabToCIFS :: FSTabEntry -> RofiIO MountConf CIFS
fstabToCIFS FSTabEntry{ fstabSpec = s, fstabDir = d, fstabOptions = o } = do
  -- If the options specify "guest" don't require a password. Else try to find a
  -- means to get the password from the command line options provided for the
  -- this mountpoint. If nothing is found, prompt for a password. In any case,
  -- the output will be passed to env variable PASSWD when mounting this cifs
  -- directory and cause it to fail. Setting the env variable is necessary as
  -- the cifs mount call will prompt for a password and hang otherwise.
  pwd <- if M.member "guest" o
         then return Nothing
         else asks $ Just . M.findWithDefault readPassword d . passwords
  let r = Removable { deviceSpec = smartSlashPrefix s, label = takeFileName d }
  return $ CIFS r d pwd
  where
    smartSlashPrefix a = if "//" `isPrefixOf` a then a else "//" ++ a

--------------------------------------------------------------------------------
-- | SSHFS Devices
--
-- This wraps the Removable device (since it is removable) and also adds its
-- own mount options. If the path does not point to an aliased entry in the ssh
-- config that specifies the port, hostname, user, and identity file, these
-- need to be passed as mount options.

data SSHFS = SSHFS Removable FilePath

instance Mountable SSHFS where
  mount (SSHFS Removable{ label = l } m) False =
    bracketOnError_
      (mkDirMaybe m)
      (rmDirMaybe m)
      (io $ runMountNotify "mount" [m] l False)

  mount (SSHFS Removable{ label = l } m) True = umountNotify l m

  allInstalled _ = fmap isJust $ io $ findExecutable "sshfs"

  isMounted (SSHFS _ dir) = io $ isDirMounted dir

  fmtEntry (SSHFS r _) = fmtEntry r

fstabToSSHFS :: FSTabEntry -> RofiIO MountConf SSHFS
fstabToSSHFS FSTabEntry{ fstabSpec = s, fstabDir = d } = return $ SSHFS r d
  where
    r = Removable { deviceSpec = s, label = takeFileName d }

--------------------------------------------------------------------------------
-- | VeraCrypt Devices
--

data VeraCrypt = VeraCrypt Removable FilePath (Maybe PasswordGetter)

instance Mountable VeraCrypt where
  mount (VeraCrypt Removable{ deviceSpec = s, label = l } m getPwd) False =
    bracketOnError_ (mkDirMaybe m) (rmDirMaybe m) mountMaybe
    where
      mountMaybe = io $ maybe (runVeraCryptWith "" []) (runVeraCryptWithPwd =<<) getPwd
      runVeraCryptWithPwd = maybe notifyFail (\p -> runVeraCryptWith p ["--stdin"])
      runVeraCryptWith stdin args = (\res -> notifyMounted (isRight res) False l)
        =<< runVeraCrypt stdin ([s, m] ++ args)
      notifyFail = notify "dialog-error-symbolic" $
        printf "Failed to get volume password for %s" l

  mount (VeraCrypt Removable{ label = l } m _) True = io $ do
    res <- runVeraCrypt "" ["-d", m]
    notifyMounted (isRight res) True l

  allInstalled _ = io $ isJust <$> findExecutable "veracrypt"

  isMounted (VeraCrypt _ dir _) = io $ isDirMounted dir

  fmtEntry (VeraCrypt r _ _) = fmtEntry r

-- NOTE: the user is assumed to have added themselves to the sudoers file so
-- that this command will work
runVeraCrypt :: String -> [String] -> IO (Either (Int, String, String) String)
runVeraCrypt stdin args = do
  readCmdEither "sudo" (defaultArgs ++ args) stdin
  where
    defaultArgs = ["/usr/bin/veracrypt", "--text", "--non-interactive"]

getVeracryptDevices :: RofiIO MountConf [VeraCrypt]
getVeracryptDevices = mapM toDev =<< asks vcMounts
  where
    toDev (d, s) = do
      pwd <- asks $ Just . M.findWithDefault readPassword d . passwords
      let r = Removable { deviceSpec = s, label = takeFileName d }
      return $ VeraCrypt r d pwd

--------------------------------------------------------------------------------
-- | MTP devices
--
-- These devices are a bit special because they are not based on Removable
-- devices (eg they don't have a label and a device spec). Instead they
-- are defined by a bus:device path. The program used for this is jmtpfs
-- (which seems to be the fastest and most robust)

data MTPFS = MTPFS
    { bus         :: String
    , device      :: String
    , mountpoint  :: FilePath
    , description :: String
    }
    deriving (Eq, Show)

instance Mountable MTPFS where
  mount MTPFS {..} False = do
    -- TODO add autodismount to options
    let dev = "-device=" ++ bus ++ "," ++ device
    bracketOnError_
      (mkDirMaybe mountpoint)
      (rmDirMaybe mountpoint)
      (io $ runMountNotify "jmtpfs" [dev, mountpoint] description False)

  mount MTPFS { mountpoint = m, description = d } True = umountNotify d m

  -- | return True always since the list won't even show without jmtpfs
  allInstalled _ = return True

  isMounted MTPFS { mountpoint = dir } = io $ isDirMounted dir

  fmtEntry MTPFS { description = d } = d

-- | Return list of all available MTP devices
getMTPDevices :: RofiIO MountConf [MTPFS]
getMTPDevices = do
  dir <- asks mountDir
  res <- io $ readProcess "jmtpfs" ["-l"] ""
  return $ fromLines (toDev dir) $ toDevList res
  where
    toDevList = reverse
      . takeWhile (not . isPrefixOf "Available devices")
      . reverse
      . lines
    toDev dir s = case splitOn ", " s of
      [busNum, devNum, _, _, desc, vendor] -> let d = unwords [vendor, desc]
        in Just $ MTPFS
           { bus = busNum
           , device = devNum
           , mountpoint = dir </> canonicalize d
           , description = d
           }
      _ -> Nothing
    canonicalize = mapMaybe repl
    repl c
      | c `elem` ("\"*/:<>?\\|" :: String) = Nothing
      | c == ' ' = Just '-'
      | otherwise = Just c

--------------------------------------------------------------------------------
-- | Mountable typeclass
--
-- Let this class represent anything that can be mounted. The end goal is to
-- create a Rofi action which will define an entry in the rofi prompt for the
-- device at hand. In order to make an action, we need functions to mount the
-- device, check if the necessary mounting program(s) is installed, make the
-- entry to go in the prompt, and test if the device is mounted.

class Mountable a where
  -- | Mount the given type (or dismount if False is passed)
  mount :: a -> Bool -> RofiIO MountConf ()

  -- | Check if the mounting utilities are present
  allInstalled :: a -> RofiIO MountConf Bool

  -- | Return a string to go in the Rofi menu for the given type
  fmtEntry :: a -> String

  -- | Determine if the given type is mounted or not
  isMounted :: a -> RofiIO MountConf Bool

  -- | Given a mountable type, return a rofi action (string to go in the
  -- Rofi prompt and an action to perform when it is selected)
  mkAction :: a -> RofiIO MountConf (String, RofiIO MountConf ())
  mkAction dev = do
    m <- isMounted dev
    i <- allInstalled dev
    let a = when i $ mount dev m
    let s = mountedPrefix m i ++ fmtEntry dev
    return (s, a)
    where
      mountedPrefix False True = "  "
      mountedPrefix True True  = "* "
      mountedPrefix _ False    = "! "

-- TODO add truecrypt volumes
--------------------------------------------------------------------------------
-- | Fstab devices
--
-- Functions to gather all user fstab mounts on the system

-- | Intermediate structure to hold fstab devices
data FSTab = FSTab
    { sshfsDevices :: [SSHFS]
    , cifsDevices  :: [CIFS]
    -- , veracryptDevices :: [VeraCrypt]
    }

-- | Data structure representing an fstab device (or one line in the fstab file)
data FSTabEntry = FSTabEntry
    { fstabSpec    :: String
    , fstabDir     :: FilePath
    , fstabType    :: String
    , fstabOptions :: MountOptions
    }

-- | Key/val pairs to represent mount options. A Nothing for the value signifies
-- a standalone option (eg 'rw' and 'ro')
type MountOptions = M.Map String (Maybe String)

-- | Return all user fstab devices from /etc/fstab
readFSTab :: RofiIO MountConf FSTab
readFSTab = do
  -- let i = FSTab { sshfsDevices = [], cifsDevices = [], veracryptDevices = []}
  let i = FSTab { sshfsDevices = [], cifsDevices = []}
  fstab <- io $ readFile "/etc/fstab"
  foldM addFstabDevice i $ fromLines toEntry $ lines fstab
  where
    toEntry line = case words line of
      (('#':_):_)                     -> Nothing
      [spec, dir, fsType, opts, _, _] -> Just $ FSTabEntry
                                         { fstabSpec = spec
                                         , fstabDir = dir
                                         , fstabType = fsType
                                         , fstabOptions = parseOptions opts
                                         }
      _                               -> Nothing
    parseOptions = M.fromList . fromCommaSepString

-- | Add entry to the fstab devices list, but only if it is a known user mount
addFstabDevice :: FSTab -> FSTabEntry -> RofiIO MountConf FSTab
addFstabDevice f@FSTab{..} e@FSTabEntry{..}
  | M.notMember "users" fstabOptions = return f
  | fstabType == "cifs" =
    (\d -> f { cifsDevices = append d cifsDevices }) <$> fstabToCIFS e
  | fstabType == "fuse.sshfs" =
    (\d -> f { sshfsDevices = append d sshfsDevices }) <$> fstabToSSHFS e
  -- | fstabType == "veracrypt" =
  --   (\d -> f { veracryptDevices = append d veracryptDevices }) <$> fstabToVeraCrypt e
  | otherwise = return f
  where
    append x xs = xs ++ [x]

--------------------------------------------------------------------------------
-- | Low-level mount functions

-- ASSUME these will never fail because the format of /proc/mounts is fixed

curMountField :: Int -> IO [String]
curMountField i = fmap ((!! i) . words) . lines <$> readFile "/proc/mounts"

curDeviceSpecs :: IO [String]
curDeviceSpecs = curMountField 0

curMountpoints :: IO [String]
curMountpoints = curMountField 1

-- ASSUME the base mount path will always be created because
-- 'createDirectoryIfMissing' will make parents if missing, and that removing
-- all the directories will leave the base mount path intact regardless of if it
-- was present before doing anything (which matters here since I'm putting the
-- base path in /tmp, so all this is saying is that umounting everything will
-- leave /tmp/media/USER without removing all the way down to /tmp)

mkDirMaybe :: FilePath -> RofiIO MountConf ()
mkDirMaybe fp = whenInMountDir fp $ io $ createDirectoryIfMissing True fp

rmDirMaybe :: FilePath -> RofiIO MountConf ()
rmDirMaybe fp = whenInMountDir fp $ unlessMountpoint fp
  $ asks mountDir >>= io . rmUntil fp
  where
    rmUntil cur target = unless (target == cur) $ do
      removePathForcibly cur
      rmUntil (takeDirectory cur) target

whenInMountDir :: FilePath -> RofiIO MountConf () -> RofiIO MountConf ()
whenInMountDir fp f = do
  mDir <- asks mountDir
  when (mDir `isPrefixOf` fp) f

unlessMountpoint :: FilePath -> RofiIO MountConf () -> RofiIO MountConf ()
unlessMountpoint fp f = do
  mounted <- io $ isDirMounted fp
  unless mounted f

isDirMounted :: FilePath -> IO Bool
isDirMounted fp = elem fp <$> curMountpoints

runMountNotify :: String -> [String] -> String -> Bool -> IO ()
runMountNotify cmd args msg mounted = do
  res <- readCmdEither cmd args ""
  notifyMounted (isRight res) mounted msg

umountNotify' :: String -> String -> FilePath -> RofiIO MountConf ()
umountNotify' cmd msg dir = finally
  (io $ runMountNotify cmd [dir] msg True)
  (rmDirMaybe dir)

umountNotify :: String -> FilePath -> RofiIO MountConf ()
umountNotify = umountNotify' "umount"

-- | Send a notification indicating the mount succeeded
notifyMounted :: Bool -> Bool -> String -> IO ()
notifyMounted succeeded mounted label = notify icon body
  where
    (format, icon) = if succeeded
      then ("Successfully %sed %s", "dialog-information-symbolic")
      else ("Failed to %s %s", "dialog-error-symbolic")
    m = if mounted then "unmount" else "mount" :: String
    body = printf format m label

notify :: String -> String -> IO ()
notify icon body = void $ spawnProcess "notify-send" ["-i", icon, body]

--------------------------------------------------------------------------------
-- | Other functions

fromLines :: (String -> Maybe a) -> [String] -> [a]
fromLines f = mapMaybe (f . stripWS)

-- TODO this exists somewhere...
splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]]
  where
    f _ [] = []
    f c l@(x:xs) | c == delimiter = []:l
                 | otherwise = (c:x):xs

-- | Like fromCommaSepString but only return substrings with '='
fromCommaSepString' :: String -> [(String, String)]
fromCommaSepString' s = [(k, v) | (k, Just v) <- fromCommaSepString s]

-- | Split a string of comma-separated values into an alist
-- If the substrings have an '=' in them, the left side will become the key and
-- the right will become the value of the cell. If there is not '=' then the
-- entire substring will become the key and the value will be Nothing
fromCommaSepString :: String -> [(String, Maybe String)]
fromCommaSepString = fmap (toCell . splitEq) . splitBy ','
  where
    splitEq e = e =~ ("=" :: String) :: (String, String, String)
    toCell (k, "=", v) = (k, Just v)
    toCell (k, _, _)   = (k, Nothing)
