{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Monad
import           Control.Monad.Reader

import           Data.Either
import           Data.List
import           Data.List.Split       (splitOn)
import qualified Data.Map              as M
import qualified Data.Map.Ordered      as O
import           Data.Maybe

import           Rofi.Command

import           Text.Printf
import           Text.Regex.TDFA

import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.Posix.User     (getEffectiveUserName)
import           System.Process

import           UnliftIO.Exception

main :: IO ()
main = check >> getArgs >>= parse

type Password = IO (Maybe String)

data MountConf = MountConf
    { credentials :: M.Map String Password
    , mountDir    :: FilePath
    , rofiArgs    :: [String]
    }

instance RofiConf MountConf where
  defArgs MountConf { rofiArgs = a } = a


type DevicePasswords = M.Map String Password

initMountConf :: [String] -> IO MountConf
initMountConf a = conf <$> getEffectiveUserName
  where
    conf u = MountConf
      { credentials = M.empty
      , mountDir = "/media" </> u
      , rofiArgs = a
      }

options :: [OptDescr (MountConf -> MountConf)]
options =
  -- TODO clean up massive text blocks here with textwrap
  [ Option ['s'] ["secret"]
    (ReqArg (\s m -> m { credentials = addGetSecret (credentials m) s } ) "SECRET")
    ("Use libsecret to retrieve password for DIR using ATTR/VAL pairs.\n" ++
     "The pairs will be supplied to a 'secret-tool lookup' call.\n" ++
     "Argument is formatted like 'DIR:ATTR1=VAL1,ATTR2=VAL2...'")
  , Option ['d'] ["directory"]
    (ReqArg (\s m -> m { mountDir = s } ) "DIR")
    ("The directory in which new mountpoints will be created. This is\n" ++
     "assumed to be writable to the current user, and will be used for\n" ++
     "fuse entries as well as user mounts in fstab. For the latter, it is\n" ++
     "assumed that all user mounts contain this directory if a mountpoint\n" ++
     "does not already exist for them. If not diven this will default to\n" ++
     "'/media/USER'.")
  , Option ['p'] ["password"]
    (ReqArg (\s m -> m { credentials = addGetPrompt (credentials m) s } ) "DIR")
    "Prompt for password when mounting DIR."
  ]

parse :: [String] -> IO ()
parse args = case getOpt Permute options args of
  (o, n, [])   -> do
    i <- initMountConf n
    runMounts $ foldl (flip id) i o
  -- TODO make this a real error
  (_, _, errs) -> putStrLn $ concat errs ++ usageInfo "header" options

addGetSecret :: DevicePasswords -> String -> DevicePasswords
addGetSecret pwds c = case splitPrefix c of
  (dir, ":", r) -> addPasswordGetter pwds dir $ runGetSecret
    $ mapMaybe (toCell . splitEq) $ splitBy ',' r
  _             -> pwds
  where
    splitPrefix s = s =~ (":" :: String) :: (String, String, String)
    splitEq e = e =~ ("=" :: String) :: (String, String, String)
    toCell (k, "=", v) = Just (k, v)
    toCell _           = Nothing

runGetSecret :: [(String, String)] -> Password
runGetSecret kvs = readCmdSuccess "secret-tool" ("lookup":kvs') ""
  where
    kvs' = concatMap (\(k, v) -> [k, v]) kvs

addGetPrompt :: DevicePasswords -> String -> DevicePasswords
addGetPrompt pwds dir = addPasswordGetter pwds dir readPassword

addPasswordGetter :: DevicePasswords -> String -> IO (Maybe String) -> DevicePasswords
addPasswordGetter pwds key f = M.insert key f pwds

-- runGetBwPwd :: [(String, String)] -> IO (Maybe String)
-- runGetBwPwd = undefined

-- getPassword :: Credentials -> IO (Maybe String)
-- getPassword NoCredentials = return Nothing
-- getPassword (Secret kvs) = do
--   let kvs' = concat [[a, b] | (a, b) <- M.toList kvs]
--   readCmdSuccess "secret-tool" ("lookup":kvs') ""

-- TODO
check :: IO ()
check = return ()

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
    align ks = fmap (intercalate alignSep)
      $ transpose
      $ mapToLast pad
      $ transpose
      $ fmap (splitOn alignSepPre) ks
    pad xs = let m = getMax xs in fmap (\x -> take m (x ++ repeat ' ')) xs
    getMax = maximum . fmap length
    mapToLast _ []     = []
    mapToLast _ [x]    = [x]
    mapToLast f (x:xs) = f x : mapToLast f xs

-- | Class and methods for type representing mountable devices
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

-- | Key/val pairs to represent mount options. A Nothing for the value signifies
-- a standalone option (eg 'rw' and 'ro')
type MountOptions = M.Map String (Maybe String)

-- | Given a string of comma-separated 'key=val' pairs, return a mount options
-- map
parseMountOptions :: String -> MountOptions
parseMountOptions s = M.fromList $ toCell . splitEq <$> splitBy ',' s
  where
    splitEq e = e =~ ("=" :: String) :: (String, String, String)
    toCell (k, "=", v) = (k, Just v)
    toCell (k, _, _)   = (k, Nothing)

-- -- | Given a mount options map, return a string of comma separated items
-- fmtMountOptions :: MountOptions -> String
-- fmtMountOptions = intercalate "," . fmap fromCell . M.toList
--   where
--     fromCell (k, Just v)  = k ++ "=" ++ v
--     fromCell (k, Nothing) = k

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
  -- | (Un)mount the device using udiskctl
  mount Removable { deviceSpec = d, label = l } m = io $ do
    res <- readCmdEither "udisksctl" [cmd, "-b", d] ""
    notifyMounted (isRight res) m l
      where
        cmd = if m then "unmount" else "mount"

  -- | Need udisksctl to mount and umount
  allInstalled _ = fmap isJust $ io $ findExecutable "udisksctl"

  -- | Check if the device is mounted using /proc/mount
  isMounted Removable { deviceSpec = d } = elem d <$> io curDeviceSpecs

  -- | Format the Rofi entry like 'LABEL - PATH' and add a star in the front
  -- if the device is mounted
  fmtEntry Removable { deviceSpec = d, label = l } = l ++ alignSepPre ++ d

-- | Return list of possible rofi actions for removable devices
-- A 'removable device' is defined as a hotplugged device with a filesystem as
-- reported by 'lsblk'. If the LABEL does not exist on the filesystem, the
-- label shown on the prompt will be 'SIZE Volume' where size is the size of
-- the device
getRemovableDevices :: RofiConf c => RofiIO c [Removable]
getRemovableDevices = mapMaybe toDev
  . lines
  . stripWS
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
-- own mount options and credentials for authentication.

data CIFS = CIFS Removable FilePath (Maybe Password)

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
  -- This is a hack. If the options specify "guest" don't require a
  -- password. Else try to find a means to get the password from the
  -- command line options provided for the this mountpoint. If nothing is
  -- found, create a dummy function that returns "" as the password, which
  -- will be passed to the env variable PASSWD when mounting this cifs
  -- directory and cause it to fail. Setting the env variable is necessary
  -- the cifs mount call will prompt for a password and hang otherwise.
  pwd <- if M.member "guest" o
         then return Nothing
         else Just . M.findWithDefault (return $ Just "") d <$> asks credentials
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
      $ io $ do
      res <- readCmdEither "mount" [m] ""
      notifyMounted (isRight res) False l

  mount (SSHFS Removable{ label = l } m) True = umountNotify l m

  allInstalled _ = fmap isJust $ io $ findExecutable "sshfs"

  isMounted (SSHFS _ dir) = io $ isDirMounted dir

  fmtEntry (SSHFS r _) = fmtEntry r

fstabToSSHFS :: FSTabEntry -> RofiIO MountConf SSHFS
fstabToSSHFS FSTabEntry{ fstabSpec = s, fstabDir = d } = return $ SSHFS r d
  where
    r = Removable { deviceSpec = s, label = takeFileName d }

--------------------------------------------------------------------------------
-- | MTP devices

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
      $ io $ do
      res <- readCmdEither "jmtpfs" [dev, mountpoint] ""
      notifyMounted (isRight res) False description

  mount MTPFS { mountpoint = m, description = d } True = umountNotify d m

  -- | return True always since the list won't even show without jmtpfs
  allInstalled _ = return True

  isMounted MTPFS { mountpoint = dir } = io $ isDirMounted dir

  fmtEntry MTPFS { description = d } = d

getMTPDevices :: RofiIO MountConf [MTPFS]
getMTPDevices = do
  dir <- asks mountDir
  res <- io $ readProcess "jmtpfs" ["-l"] ""
  return $ mapMaybe (toDev dir) $ toDevList res
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

-- TODO add truecrypt volumes (see tcplay, will need root)
--------------------------------------------------------------------------------
-- | Fstab devices

data FSTab = FSTab
    { sshfsDevices :: [SSHFS]
    , cifsDevices  :: [CIFS]
    }

data FSTabEntry = FSTabEntry
    { fstabSpec    :: String
    , fstabDir     :: FilePath
    , fstabType    :: String
    , fstabOptions :: MountOptions
    }

readFSTab :: RofiIO MountConf FSTab
readFSTab = do
  let i = FSTab { sshfsDevices = [], cifsDevices = [] }
  fstab <- io $ readFile "/etc/fstab"
  foldM addFstabDevice i $ mapMaybe toEntry $ lines fstab
  where
    toEntry line = case words $ stripWS line of
      (('#':_):_)                     -> Nothing
      [spec, dir, fsType, opts, _, _] -> Just $ FSTabEntry
                                         { fstabSpec = spec
                                         , fstabDir = dir
                                         , fstabType = fsType
                                         , fstabOptions = parseMountOptions opts
                                         }
      _                               -> Nothing

addFstabDevice :: FSTab -> FSTabEntry -> RofiIO MountConf FSTab
addFstabDevice f@FSTab{..} e@FSTabEntry{..}
  | M.notMember "users" fstabOptions = return f
  | fstabType == "cifs" =
    (\d -> f { cifsDevices = cifsDevices ++ [d] }) <$> fstabToCIFS e
  | fstabType == "fuse.sshfs" =
    (\d -> f { sshfsDevices = sshfsDevices ++ [d] }) <$> fstabToSSHFS e
  | otherwise = return f

--------------------------------------------------------------------------------
-- | Low-level mount functions

-- ASSUME these will never fail because the format of this file is fixed

curMountField :: Int -> IO [String]
curMountField i = fmap ((!! i) . words) . lines <$> readFile "/proc/mounts"

curDeviceSpecs :: IO [String]
curDeviceSpecs = curMountField 0

curMountpoints :: IO [String]
curMountpoints = curMountField 1

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

umountNotify :: String -> FilePath -> RofiIO MountConf ()
umountNotify label dir = finally cmd $ rmDirMaybe dir
  where
    cmd = io $ do
      res <- readCmdEither "umount" [dir] ""
      notifyMounted (isRight res) True label

isDirMounted :: FilePath -> IO Bool
isDirMounted fp = elem fp <$> curMountpoints

--------------------------------------------------------------------------------
-- | Other functions

-- TODO this exists somewhere...
splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]]
  where
    f _ [] = []
    f c l@(x:xs) | c == delimiter = []:l
                 | otherwise = (c:x):xs

notifyMounted :: Bool -> Bool -> String -> IO ()
notifyMounted succeeded mounted label = void $ spawnProcess "notify-send" [msg]
  where
    f = if succeeded then "Successfully %sed %s" else "Failed to %s %s"
    m = if mounted then "unmount" else "mount" :: String
    msg = printf f m label
