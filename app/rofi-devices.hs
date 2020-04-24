{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Exception
import           Control.Monad

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.Csv                   hiding (lookup)
import           Data.Either
import           Data.List
import           Data.List.Split            (splitOn)
import qualified Data.Map                   as M
import qualified Data.Map.Ordered           as O
import           Data.Maybe
import           Data.Vector                (Vector, toList)

import           GHC.Generics               (Generic)

import           Rofi.Command

import           Text.Printf
import           Text.Regex.TDFA

import           System.Directory
import           System.Environment
import           System.Process

main :: IO ()
main = check >> getArgs >>= parse

parse :: [String] -> IO ()
parse = runMounts

-- TODO
check :: IO ()
check = return ()

runMounts :: [String] -> IO ()
runMounts a = do
  let c = RofiConf { defArgs = a }
  runRofiPrompt c runPrompt

runPrompt :: RofiPrompt ()
runPrompt = do
  net <- titledGroup "Network Devices" <$> io getNetActions
  rmv <- titledGroup "Removable Devices" <$> io getRemovableActions
  mtp <- titledGroup "MTP Devices" <$> io getMTPActions
  selectAction $ emptyMenu
        { groups = [net, rmv, mtp]
        , prompt = Just "Select Device"
        }

getNetActions :: IO RofiActions
getNetActions = alignEntries . toRofiActions . catMaybes
  <$> (mapM csvToAction =<< getCSV)

alignSep :: String
alignSep = " | "

alignSepPre :: String
alignSepPre = "@@@"

alignEntries :: RofiActions -> RofiActions
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
  mount :: a -> Bool -> IO ()

  -- | Check if the mounting utilities are present
  allInstalled :: a -> IO Bool

  -- | Return a string to go in the Rofi menu for the given type
  fmtEntry :: a -> String

  -- | Determine if the given type is mounted or not
  isMounted :: a -> IO Bool

  -- | Given a mountable type, return a rofi action (string to go in the
  -- Rofi prompt and an action to perform when it is selected)
  mkAction :: a -> IO (String, RofiPrompt ())
  mkAction dev = do
    m <- isMounted dev
    i <- allInstalled dev
    let a = when i $ io $ mount dev m
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

-- | Given a mount options map, return a string of comma separated items
fmtMountOptions :: MountOptions -> String
fmtMountOptions = intercalate "," . fmap fromCell . M.toList
  where
    fromCell (k, Just v)  = k ++ "=" ++ v
    fromCell (k, Nothing) = k

-- | Various credentials to be used with a given mountable type.
-- Secret: represents a lookup using 'secret-tool' where the map represents
-- the attribute/value pairs to pass.
-- NoCredentials: self explanatory
data Credentials = Secret (M.Map String String)
    | NoCredentials
    deriving (Eq, Show)

-- | Given a string, return a credentials type. The type of credentials is
-- determined by the prefix (which is followed by a colon) and is followed by
-- a comma-separated list of 'key=val' pairs
parseCredentials :: String -> Credentials
parseCredentials c = case splitPrefix c of
  ("secret", ":", r) -> Secret $ M.fromList $ mapMaybe (toCell . splitEq) $ splitBy ',' r
  -- TODO fetch from bitwarden
  -- add more here...
  _                  -> NoCredentials
  where
    splitPrefix s = s =~ (":" :: String) :: (String, String, String)
    splitEq e = e =~ ("=" :: String) :: (String, String, String)
    toCell (k, "=", v) = Just (k, v)
    toCell _           = Nothing

--------------------------------------------------------------------------------
-- | Removable devices
--
-- A device which can be removed (which is all the devices we care about)
-- This can be minimally described by a device PATH and LABEL. If MOUNTPOINT is
-- Nothing, this represents the device being mounted at a default location.

data Removable = Removable
    { path       :: String
    , label      :: String
    , mountpoint :: Maybe String
    }
    deriving (Eq, Show)

instance Mountable Removable where
  -- | (Un)mount the device using udiskctl
  mount Removable { path = p, label = l } m = do
    res <- readCmdEither "udisksctl" [cmd, "-b", p] ""
    notifyMounted (isRight res) m l
      where
        cmd = if m then "unmount" else "mount"

  -- | Need udisksctl to mount and umount
  allInstalled _ = isJust <$> findExecutable "udisksctl"

  -- | Check if the device is mounted using /proc/mount
  isMounted Removable { path = p, mountpoint = m } = do
    cur <- curMountpoints
    return $ case m of
      Just m' -> (p, m') `elem` cur
      Nothing -> p `elem` fmap fst cur

  -- | Format the Rofi entry like 'LABEL - PATH' and add a star in the front
  -- if the device is mounted
  fmtEntry Removable { path = p, label = l } = l ++ alignSepPre ++ p


-- | Return list of possible rofi actions for removable devices
-- A 'removable device' is defined as a hotplugged device with a filesystem as
-- reported by 'lsblk'. If the LABEL does not exist on the filesystem, the
-- label shown on the prompt will be 'SIZE Volume' where size is the size of
-- the device
getRemovableDevices :: IO [Removable]
getRemovableDevices = mapMaybe toDev
  . lines
  . stripWS
  <$> readProcess "lsblk" ["-n", "-r", "-o", columns] ""
  where
    columns = "FSTYPE,HOTPLUG,PATH,LABEL,SIZE"
    -- can't use 'words' here since it will drop spaces in the front
    toDev line = case splitBy ' ' line of
      ("":_)             -> Nothing
      [_, "1", p, "", s] -> mk p $ s ++ " Volume"
      [_, "1", p, l, _]  -> mk p l
      _                  -> Nothing
    mk p l = Just $ Removable { path = p
                              , label = l
                              , mountpoint = Nothing
                              }

getRemovableActions :: IO RofiActions
getRemovableActions = alignEntries . toRofiActions
  <$> (mapM mkAction =<< getRemovableDevices)

--------------------------------------------------------------------------------
-- | CIFS Devices
--
-- This wraps the Removable device (since it is removable) and also adds its
-- own mount options and credentials for authentication.

data CIFS = CIFS Removable MountOptions Credentials

instance Mountable CIFS where
  -- | Mount using udevil
  mount (CIFS Removable{..} opts creds) False = do
    pwd <- getPassword creds
    let opts' = fmtOpts $ addPwd pwd opts
    res <- readCmdEither "udevil" (["mount"] ++ opts' ++ ["-t", "cifs", path]) ""
    notifyMounted (isRight res) False label
    where
      addPwd (Just pwd) o = M.insert "password" (Just pwd) o
      addPwd Nothing o    = o
      fmtOpts o = if null o then [] else ["-o", fmtMountOptions o]

  -- | Unmount using udevil
  mount (CIFS Removable{..} _ _) True = do
    res <- readCmdEither "udevil" ["unmount", path] ""
    notifyMounted (isRight res) True label

  -- | Need udevil and mount.cifs
  allInstalled _ = all isJust <$> mapM findExecutable ["udevil", "mount.cifs"]

  -- | Return True if mounted. Only checks the removable type wrapped within
  isMounted (CIFS r _ _) = isMounted r

  -- | Format the Rofi entry like 'LABEL - (CIFS) - PATH' and prefix with a star
  -- if mounted
  fmtEntry (CIFS r _ _) = fmtNetEntry r "CIFS"

--------------------------------------------------------------------------------
-- | SSHFS Devices
--
-- This wraps the Removable device (since it is removable) and also adds its
-- own mount options. If the path does not point to an aliased entry in the ssh
-- config that specifies the port, hostname, user, and identity file, these
-- need to be passed as mount options.

data SSHFS = SSHFS Removable MountOptions

instance Mountable SSHFS where
  -- | Mount using sshfs
  mount (SSHFS Removable{..} opts) False =
    case mountpoint of
      Just m  -> cmd m
      -- TODO only destroy mountpoint if it is not already another mountpoint
      Nothing -> bracketOnError (makeFuseMount label)
        (const $ destroyFuseMount label) (const $ cmd $ fmtFusePath label)
    where
      -- TODO add auto-dismount to options
      opts' = if null opts then [] else ["-o", fmtMountOptions opts]
      cmd m' = do
        res <- readCmdEither "sshfs" ([path, m'] ++ opts') ""
        notifyMounted (isRight res) False label

  -- | Umount using fusermount
  mount (SSHFS r _) True = fuseUnmount r

  -- | Need sshfs (assume fuse is also installed)
  allInstalled _ = isJust <$> findExecutable "sshfs"

  -- | Return True if mounted. Only checks the removable type wrapped within
  isMounted (SSHFS r _) = isMounted r

  -- | Format the Rofi entry like 'LABEL - (SSHFS) - PATH' and prefix with a
  -- star if mounted
  fmtEntry (SSHFS r _) = fmtNetEntry r "SSHFS"

-- | Given a removable device, type string, and boolean for if the device is
-- mounted, return a string like 'LABEL - (TYPESTRING) - PATH' and prefix with a
-- star if mounted
fmtNetEntry :: Removable -> String -> String
fmtNetEntry Removable { label = l, path = p } t =
  intercalate alignSepPre [l, t, p]

--------------------------------------------------------------------------------
-- | MTP devices

data MTPFS = MTPFS
    { bus           :: String
    , device        :: String
    , mountpointMTP :: String
    , description   :: String
    }
    deriving (Eq, Show)

instance Mountable MTPFS where
  -- | Mount using sshfs
  mount MTPFS {..} False = do
    -- TODO add autodismount to options
    let dev = "-device=" ++ bus ++ "," ++ device
    bracketOnError (createDirectoryIfMissing False mountpointMTP)
      (const $ removePathForcibly mountpointMTP) $ \_ -> do
      res <- readCmdEither "jmtpfs" [dev, mountpointMTP] ""
      notifyMounted (isRight res) False description

  -- | Umount using fusermount
  mount MTPFS { mountpointMTP = m, description = d } True =
    finally (fuseUnmount' d m) $ removePathForcibly m

  -- | Need jmtpfs (assume fuse is also installed)
  allInstalled _ = isJust <$> findExecutable "jmtpfs"

  -- | Return True if mounted. Only checks the mountpoint path
  isMounted MTPFS { mountpointMTP = m } = elem m . fmap snd <$> curMountpoints

  -- | Format the Rofi entry like 'LABEL - (SSHFS) - PATH'
  fmtEntry MTPFS { description = d } = d

getMTPDevices :: IO [MTPFS]
getMTPDevices = mapMaybe toDev . toDevList <$> readProcess "jmtpfs" ["-l"] ""
  where
    toDevList = reverse
      . takeWhile (not . isPrefixOf "Available devices")
      . reverse
      . lines
    toDev s = case splitOn ", " s of
      [busNum, devNum, _, _, desc, vendor] -> let d = unwords [vendor, desc]
        in Just $ MTPFS
           { bus = busNum
           , device = devNum
           , mountpointMTP = fuseMount ++ canonicalize d
           , description = d
           }
      _ -> Nothing
    canonicalize = mapMaybe repl
    repl c
      | c `elem` ("\"*/:<>?\\|" :: String) = Nothing
      | c == ' ' = Just '-'
      | otherwise = Just c

getMTPActions :: IO RofiActions
getMTPActions = toRofiActions <$> (mapM mkAction =<< getMTPDevices)

-- TODO add truecrypt volumes (see tcplay, will need root)
--------------------------------------------------------------------------------
-- | Csv device parsing
--
-- These devices are stored in a CSV file which needs to be parsed into the
-- appropriate devices types

-- | Represents one parsable line in the network device config .tsv file
data CsvDev = CsvDev
    { csvLabel        :: String
    , csvType         :: String
    , csvPath         :: String
    , csvMountpoint   :: Maybe String
    , csvMountOptions :: Maybe String
    , csvCredentials  :: Maybe String
    }
    deriving (Generic, Show)

instance FromRecord CsvDev

-- | Return a list of all Csv lines from the network config file
getCSV :: IO [CsvDev]
getCSV = do
  xdgConf <- getEnv "XDG_CONFIG_HOME"
  -- TODO this shouldn't be hardcoded
  contents <- B.readFile $ xdgConf ++ "/rofi/devices.tsv"
  let opts = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }
  case decodeWith opts HasHeader contents of
    Left s  -> putStrLn s >> return []
    Right v -> return $ toList (v :: Vector CsvDev)

-- TODO split this into each device type so they can be separated in the prompt
-- | Given a parsed csv line from the network config file, return a
-- (ENTRY, ACTION) where ENTRY is a string to appear on the Rofi prompt and
-- ACTION is an action to perform on the device in the csv line when selected
csvToAction :: CsvDev -> IO (Maybe (String, RofiPrompt ()))
csvToAction CsvDev {..}
  | csvType == "cifs" = Just <$> mkAction (CIFS r' opts creds)
  | csvType == "sshfs" = Just <$> mkAction (SSHFS r opts)
  | otherwise = return Nothing
  where
    r = Removable { label = csvLabel
                  , path = csvPath
                  , mountpoint = csvMountpoint
                  }
    opts = maybe M.empty parseMountOptions csvMountOptions
    creds = maybe NoCredentials parseCredentials csvCredentials
    -- CIFS prefixes the path with two slashes
    r' = r { path = smartSlashPrefix csvPath }
    smartSlashPrefix s = if "//" `isPrefixOf` s then s else "//" ++ s

--------------------------------------------------------------------------------
-- | Low-level mount functions

-- | Return a list of mountpoints like (PATH, MOUNTPOINT) from /proc/mount
curMountpoints :: IO [(String, String)]
curMountpoints = do
  m <- readFile "/proc/mounts"
  -- ASSUME this will never fail because the format of this file is fixed
  return $ (\(path:mntpnt:_) -> (path, mntpnt)) . words <$> lines m

-- | Given a path, return its mountpoint if it exists
lookupMountpoint :: String -> IO (Maybe String)
lookupMountpoint path = lookup path <$> curMountpoints

-- | Given a removable device, unmount it using fuse
fuseUnmount :: Removable -> IO ()
fuseUnmount Removable { path = p, mountpoint = m, label = l } =
  maybe umountDef umount m
  where
    umount = fuseUnmount' l
    umountDef = lookupMountpoint p >>=
      mapM_ (liftM2 finally umount removePathForcibly)

fuseUnmount' :: String -> String -> IO ()
fuseUnmount' label path = do
  res <- readCmdEither "fusermount" ["-u", path] ""
  notifyMounted (isRight res) True label

-- | Given credentials, return a password
getPassword :: Credentials -> IO (Maybe String)
getPassword NoCredentials = return Nothing
getPassword (Secret kvs) = do
  let kvs' = concat [[a, b] | (a, b) <- M.toList kvs]
  readCmdSuccess "secret-tool" ("lookup":kvs') ""

-- TODO this shouldn't be hardcoded
fuseMount :: FilePath
fuseMount = "/media/ndwar-fuse/"

-- TODO what if there is no trailing slash?
fmtFusePath :: String -> String
fmtFusePath label = fuseMount ++ label

makeFuseMount :: String -> IO ()
makeFuseMount label = createDirectoryIfMissing False $ fmtFusePath label

destroyFuseMount :: String -> IO ()
destroyFuseMount label = removePathForcibly $ fmtFusePath label

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
