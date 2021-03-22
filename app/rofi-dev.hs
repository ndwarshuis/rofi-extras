{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

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
-- import qualified Data.Map.Ordered      as O
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Yaml

import           Rofi.Command

import           Text.Printf

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
  (o, n, [])   -> runMounts $ foldl (flip id) (defaultOpts n) o
  (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo h options
  where
    h = "Usage: rofi-dev [OPTIONS] [-- ROFI-OPTIONS]"
    defaultOpts r = Opts
      { optsConfig = Nothing
      , optsAlias = Nothing
      , optsUnmount = False
      , optsRofiArgs = r
      }

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option ['c'] ["config"]
    (ReqArg (\s m -> m { optsConfig = Just s } ) "CONF")
    "The path to the config file"
  , Option ['m'] ["mount"]
    (ReqArg (\s m -> m { optsAlias = Just s } ) "ALIAS")
    "Mount the device specified by ALIAS directly"
  , Option ['u'] ["unmount"] (NoArg (\m -> m { optsUnmount = True } ))
    "Unmount the device specified by ALIAS instead of mounting it."
  ]

data Opts = Opts
  { optsConfig   :: Maybe FilePath
  , optsAlias    :: Maybe String
  , optsUnmount  :: Bool
  , optsRofiArgs :: [String]
  } deriving Show

--------------------------------------------------------------------------------
-- | Static configuration
--
-- This is defined in a YAML file which describes how to mount each device. Here
-- I define a parser for said YAML file

defaultTries :: Integer
defaultTries = 2

(.:&) :: FromJSON a => Object -> T.Text -> Parser (V.Vector a)
(.:&) o t = o .:? t .!= V.empty

data MountConfig = MountConfig
  { _mountMountPoint :: FilePath
  , _mountLabel :: Maybe String
  } deriving Show

instance FromJSON MountConfig where
  parseJSON = withObject "devices" $ \o -> MountConfig
    <$> o .: "mountpoint"
    <*> o .:? "label"

data BitwardenConfig = BitwardenConfig
  { _bitwardenKey :: String
  , _bitwardenTries :: Integer }
  deriving Show

instance FromJSON BitwardenConfig where
  parseJSON = withObject "bitwarden" $ \o -> BitwardenConfig
    <$> o .: "key"
    <*> o .:? "tries" .!= defaultTries

newtype LibSecretConfig = LibSecretConfig
  { _libsecretAttributes :: M.Map String String }
  deriving Show

instance FromJSON LibSecretConfig where
  parseJSON = withObject "libsecret" $ \o -> LibSecretConfig
    <$> o .: "attributes"

newtype PromptConfig = PromptConfig
  { _promptTries :: Integer }
  deriving Show

instance FromJSON PromptConfig where
  parseJSON = withObject "libsecret" $ \o -> PromptConfig
    <$> o .: "tries" .!= defaultTries

data PasswordConfig = PasswordConfig
  { _passwordBitwarden :: Maybe BitwardenConfig
  , _passwordLibSecret :: Maybe LibSecretConfig
  , _passwordPrompt :: Maybe PromptConfig
  }
  deriving Show

instance FromJSON PasswordConfig where
  parseJSON = withObject "password" $ \o -> PasswordConfig
    <$> o .:? "bitwarden"
    <*> o .:? "libsecret"
    <*> o .:? "prompt"

data DataConfig = VeracryptConfig
  { _veracryptVolume :: String
  , _veracryptPassword :: Maybe PasswordConfig
  } | SSHFSConfig
  { _sshfsRemote :: String
  } | CIFSConfig
  { _cifsRemote :: String
  , _cifsPassword :: Maybe PasswordConfig
  } deriving Show

data DeviceConfig = DeviceConfig
  { _deviceMount :: MountConfig
  , _deviceData :: DataConfig
  } deriving Show

data TreeConfig = TreeConfig
  { _treeParent :: DeviceConfig
  , _treeChildren :: V.Vector String
  } deriving Show

instance FromJSON TreeConfig where
  parseJSON = withObject "devices" $ \o -> do
    devType <- o .: "type"
    deps <- o .:& "depends"
    mountconf <- o .: "mount"
    devData <- case (devType :: String) of
                 "cifs"      -> CIFSConfig
                   <$> o .: "remote"
                   <*> o .:? "password"
                 "sshfs"     -> SSHFSConfig
                   <$> o .: "remote"
                 "veracrypt" -> VeracryptConfig
                   <$> o .: "volume"
                   <*> o .:? "password"
                 -- TODO make this skip adding an entry to the map rather than
                 -- skipping the map entirely
                 _           -> fail $ "unknown device type: " ++ devType
    return $ TreeConfig
      { _treeParent = DeviceConfig
                      { _deviceMount = mountconf
                      , _deviceData = devData
                      }
      , _treeChildren = deps
      }

data StaticConfig = StaticConfig
  { _staticconfigTmpPath :: Maybe String
  , _staticconfigDevices :: M.Map String TreeConfig
  } deriving Show

instance FromJSON StaticConfig where
  parseJSON = withObject "devices" $ \o -> StaticConfig
    <$> o .:? "mountdir"
    <*> o .: "devices"

--------------------------------------------------------------------------------
-- | Global config used in the reader monad stack
--
-- This is defined by the mount options on the command line, and holds:
-- - a map between mountpoints and a means to get passwords when mounting those
--   mountpoints
-- - a mount directory where mountpoints will be created if needed (defaults
--   to '/tmp/media/USER'
-- - any arguments to be passed to the rofi command

data MountConf = MountConf
    { mountconfVolatilePath :: FilePath
    , mountconfRofiArgs     :: [String]
    , mountconfStaticDevs   :: M.Map String TreeConfig
    }

instance RofiConf MountConf where
  defArgs MountConf { mountconfRofiArgs = a } = a

--------------------------------------------------------------------------------
-- | Password-getting functions

type PasswordGetter = IO (Maybe String)

runSecret :: [(String, String)] -> PasswordGetter
runSecret kvs = readCmdSuccess "secret-tool" ("lookup":kvs') ""
  where
    kvs' = concatMap (\(k, v) -> [k, v]) kvs

runBitwarden :: String -> PasswordGetter
runBitwarden pname = ((password . login) <=< find (\i -> name i == pname))
  <$> getItems

runPromptLoop :: Integer -> PasswordGetter -> PasswordGetter
runPromptLoop n pwd = do
  res <- pwd
  if isNothing res then
    if n <= 0 then return Nothing else runPromptLoop (n-1) pwd
    else return res

configToPwd :: PasswordConfig -> PasswordGetter
configToPwd PasswordConfig{ _passwordBitwarden = b
                          , _passwordLibSecret = s
                          , _passwordPrompt = p
                          } =
    getBW b `runMaybe` getLS s `runMaybe` getPrompt p
  where
    getBW (Just BitwardenConfig{ _bitwardenKey = k, _bitwardenTries = n }) =
      runPromptLoop n $ runBitwarden k
    getBW _ = return Nothing
    getLS (Just LibSecretConfig{ _libsecretAttributes = a }) =
      runSecret $ M.toList a
    getLS _ = return Nothing
    getPrompt (Just PromptConfig{ _promptTries = n }) =
      runPromptLoop n readPassword
    getPrompt _ = return Nothing
    runMaybe x y = do
      res <- x
      if isNothing res then y else return res

--------------------------------------------------------------------------------
-- | Main prompt
--
-- This command will have one Rofi prompt and will display all available
-- mounts grouped by device type (eg removable, sshfs, cifs, etc). I like
-- pretty things, so ensure the entries are aligned properly as well

runMounts :: Opts -> IO ()
runMounts opts = do
  static <- join <$> traverse parseStaticConfig (optsConfig opts)
  defaultTmpPath <- ("/tmp/media" </>) <$> getEffectiveUserName
  let tmpPath = fromMaybe defaultTmpPath (_staticconfigTmpPath =<< static)
  let staticDevs = maybe M.empty _staticconfigDevices static
  let mountconf = MountConf
        { mountconfVolatilePath = tmpPath
        , mountconfRofiArgs = optsRofiArgs opts
        , mountconfStaticDevs = staticDevs
        }
  let byAlias = mountByAlias $ optsUnmount opts
  let byPrompt = runPrompt =<< getGroups
  runRofiIO mountconf $ maybe byPrompt byAlias $ optsAlias opts

parseStaticConfig :: FilePath -> IO (Maybe StaticConfig)
parseStaticConfig p = do
  res <- decodeFileEither p
  case res of
    Left e  -> print e >> return Nothing
    Right c -> return $ Just c

runPrompt :: RofiConf c => [RofiGroup c] -> RofiIO c ()
runPrompt gs = selectAction $ emptyMenu
               { groups = gs
               , prompt = Just "Select Device"
               }

getGroups :: RofiIO MountConf [RofiGroup MountConf]
getGroups = do
  staticDevs <- asks mountconfStaticDevs
  staticActions <- mapM mkAction $ configToTree' staticDevs
  removableActions <- mapM mkAction =<< getRemovableDevices
  mtpActions <- mapM mkAction =<< getMTPDevices
  return $ mapMaybe mkGroup
    $ groupBy (\(hx, _, _) (hy, _, _) -> hx == hy)
    $ sortBy (\(hx, _, _) (hy, _, _) -> compare hx hy)
    $ staticActions ++ removableActions ++ mtpActions

mountByAlias :: Bool -> String -> RofiIO MountConf ()
mountByAlias unmountFlag alias = do
  static <- asks mountconfStaticDevs
  mapM_ (`mount` unmountFlag) $ configToTree static <$> M.lookup alias static

mkGroup :: [(Header, String, RofiIO MountConf ())] -> Maybe (RofiGroup MountConf)
mkGroup [] = Nothing
mkGroup as = let ((Header title _, _, _):_) = as in
  -- Just $ titledGroup title $ alignEntries $ toRofiActions $ fmap (\(_, e, a) -> (e, a)) as
  Just $ titledGroup title $ toRofiActions $ fmap (\(_, e, a) -> (e, a)) as

-- alignSep :: String
-- alignSep = " | "

alignSepPre :: String
alignSepPre = "@@@"

-- alignEntries :: RofiActions c -> RofiActions c
-- alignEntries = O.fromList . withKeys . O.assocs
--   where
--     withKeys as = let (ks, vs) = unzip as in zip (align ks) vs
--     align = fmap (intercalate alignSep)
--       . transpose
--       . mapToLast pad
--       . transpose
--       . fmap (splitOn alignSepPre)
--     pad xs = let m = getMax xs in fmap (\x -> take m (x ++ repeat ' ')) xs
--     getMax = maximum . fmap length
--     mapToLast _ []     = []
--     mapToLast _ [x]    = [x]
--     mapToLast f (x:xs) = f x : mapToLast f xs

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

  getLabel Removable { label = l } = l

instance Actionable Removable where
  fmtEntry Removable { deviceSpec = d, label = l } = l ++ alignSepPre ++ d

  groupHeader _ = Header "Removable Devices" 3

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
-- | VeraCrypt Devices
--

-- NOTE: the user is assumed to have added themselves to the sudoers file so
-- that this command will work
runVeraCrypt :: String -> [String] -> IO (Either (Int, String, String) String)
runVeraCrypt stdin args = do
  readCmdEither "sudo" (defaultArgs ++ args) stdin
  where
    defaultArgs = ["/usr/bin/veracrypt", "--text", "--non-interactive"]

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

  getLabel = fmtEntry

instance Actionable MTPFS where
  fmtEntry MTPFS { description = d } = d

  groupHeader _ = Header "MTP Devices" 5

-- | Return list of all available MTP devices
getMTPDevices :: RofiIO MountConf [MTPFS]
getMTPDevices = do
  dir <- asks mountconfVolatilePath
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
-- | Static Device Wrapper
--
-- In order to make a consistent interface for the static device types, create
-- a wrapper type to encapsulate them.

data Tree a = Tree a [Tree a] deriving (Eq, Show)

type StaticConfigTree = Tree DeviceConfig

instance Actionable (Tree DeviceConfig) where
  fmtEntry (Tree p _) = getLabel p

  groupHeader (Tree DeviceConfig{ _deviceData = d } _) =
    case d of
      CIFSConfig{} -> Header "CIFS Devices" 0
      SSHFSConfig{} -> Header "SSHFS Devices" 1
      VeracryptConfig{} -> Header "Veracrypt Devices" 2

configToTree' :: M.Map String TreeConfig -> [StaticConfigTree]
configToTree' devMap = configToTree devMap <$> M.elems devMap

configToTree :: M.Map String TreeConfig -> TreeConfig -> StaticConfigTree
configToTree devMap TreeConfig{ _treeParent = p, _treeChildren = c } =
  Tree p $ fmap go V.toList c
  where
    go ds = configToTree devMap <$> mapMaybe (`M.lookup` devMap) ds

instance Mountable a => Mountable (Tree a) where
  mount (Tree p cs) False = mapM_ (`mount` False) cs >> mount p False
  mount (Tree p _) True = mount p True

  isMounted (Tree p _) = isMounted p

  allInstalled (Tree p cs) = do
    res <- and <$> mapM allInstalled cs
    if res then allInstalled p else return res

  getLabel (Tree p _) = getLabel p

instance Mountable DeviceConfig where
  mount c@DeviceConfig{ _deviceMount = MountConfig { _mountMountPoint = m }
                      , _deviceData = devData
                      } False = do
    mountRoot <- asks mountconfVolatilePath
    let m' = appendRoot mountRoot m
    bracketOnError_ (mkDirMaybe m') (rmDirMaybe m') $ mount' m'
    where
      mount' mountpoint = io $ case devData of
        SSHFSConfig{ _sshfsRemote = r } ->
          runMountNotify "sshfs" [r, mountpoint] (getLabel c) False
        CIFSConfig{ _cifsPassword = p } -> do
          res <- case p of
            Just pwd -> do
              pwd' <- maybe [] (\p' -> [("PASSWD", p')]) <$> configToPwd pwd
              readCmdEither' "mount" [mountpoint] "" pwd'
            Nothing -> readCmdEither "mount" [mountpoint] ""
          notifyMounted (isRight res) False (getLabel c)
        VeracryptConfig{ _veracryptPassword = getPwd, _veracryptVolume = v } ->
          maybe (runVeraCryptWith "" []) (runVeraCryptWithPwd =<<) (configToPwd <$> getPwd)
          where
            l = getLabel c
            runVeraCryptWithPwd = maybe notifyFail (\p -> runVeraCryptWith p ["--stdin"])
            runVeraCryptWith stdin args = (\res -> notifyMounted (isRight res) False l)
              =<< runVeraCrypt stdin ([v, mountpoint] ++ args)
            notifyFail = notify "dialog-error-symbolic" $
              printf "Failed to get volume password for %s" l

  mount c@DeviceConfig{ _deviceMount = MountConfig{ _mountMountPoint = m }
                      , _deviceData = VeracryptConfig{}
                      } True = io $ do
    res <- runVeraCrypt "" ["-d", m]
    notifyMounted (isRight res) True (getLabel c)
  
  mount c@DeviceConfig{ _deviceMount = MountConfig { _mountMountPoint = m }} True =
    umountNotify (getLabel c) m

  allInstalled DeviceConfig{ _deviceData = devData } = io $ isJust
    <$> findExecutable (exe devData)
    where
      exe SSHFSConfig{} = "sshfs"
      exe CIFSConfig{} = "mount.cifs"
      exe VeracryptConfig{} = "veracrypt"

  isMounted DeviceConfig{ _deviceMount = MountConfig{ _mountMountPoint = m }} = io $ isDirMounted m

  getLabel DeviceConfig{ _deviceMount = MountConfig{ _mountMountPoint = p, _mountLabel = l }} =
    fromMaybe (takeBaseName p) l

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

  mountMaybe :: a -> Bool -> RofiIO MountConf ()
  mountMaybe dev mountFlag = do
    mounted <- isMounted dev
    if mountFlag == mounted then mount dev mountFlag
      else io $ notify "dialog-information-symbolic"
           $ getLabel dev ++ " already mounted"

  -- | Check if the mounting utilities are present
  allInstalled :: a -> RofiIO MountConf Bool

  -- | Return a string representing the label of the device
  getLabel :: a -> String

  -- | Determine if the given type is mounted or not
  isMounted :: a -> RofiIO MountConf Bool

data Header = Header String Integer deriving (Show, Eq)

instance Ord Header where
  compare (Header _ x) (Header _ y) = compare x y

class Mountable a => Actionable a where
  -- | Return a string to go in the Rofi menu for the given type
  fmtEntry :: a -> String
  fmtEntry = getLabel

  groupHeader :: a -> Header

  -- | Given a mountable type, return a rofi action (string to go in the
  -- Rofi prompt and an action to perform when it is selected)
  mkAction :: a -> RofiIO MountConf (Header, String, RofiIO MountConf ())
  mkAction dev = do
    m <- isMounted dev
    i <- allInstalled dev
    let h = groupHeader dev
    let a = when i $ mountMaybe dev m
    let s = mountedPrefix m i ++ fmtEntry dev
    return (h, s, a)
    where
      mountedPrefix False True = "  "
      mountedPrefix True True  = "* "
      mountedPrefix _ False    = "! "

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
  $ asks mountconfVolatilePath >>= io . rmUntil fp
  where
    rmUntil cur target = unless (target == cur) $ do
      removePathForcibly cur
      rmUntil (takeDirectory cur) target

whenInMountDir :: FilePath -> RofiIO MountConf () -> RofiIO MountConf ()
whenInMountDir fp f = do
  mDir <- asks mountconfVolatilePath
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

appendRoot :: FilePath -> FilePath -> FilePath
appendRoot root path = if isRelative path then root </> path else path
