{-# LANGUAGE OverloadedStrings #-}
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

import           Data.List
import           Data.List.Split       (splitOn)
import qualified Data.Map              as M
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
-- | Main prompt
--
-- This command will have one Rofi prompt and will display all available
-- mounts grouped by device type (eg removable, sshfs, cifs, etc). I like
-- pretty things, so ensure the entries are aligned properly as well

runMounts :: Opts -> IO ()
runMounts opts = do
  static <- join <$> traverse parseStaticConfig (optsConfig opts)
  defaultTmpPath <- ("/tmp/media" </>) <$> getEffectiveUserName
  let tmpPath = fromMaybe defaultTmpPath $ staticconfigTmpPath =<< static
  let staticDevs = maybe M.empty staticconfigDevices static
  let verbose = fromMaybe False $ staticconfigVerbose =<< static
  let mountconf = MountConf
        { mountconfVolatilePath = tmpPath
        , mountconfRofiArgs = optsRofiArgs opts
        , mountconfStaticDevs = staticDevs
        , mountconfVerbose = verbose
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

getGroups :: RofiMountIO [RofiGroup MountConf]
getGroups = do
  actions <- sequence [getStaticActions, getRemovableActions, getMTPActions]
  return $ mapMaybe mkGroup
    $ groupBy (\(hx, _) (hy, _) -> hx == hy)
    $ sortBy (\(hx, _) (hy, _) -> compare hx hy)
    $ concat actions

mountByAlias :: Bool -> String -> RofiMountIO ()
mountByAlias unmountFlag alias = do
  static <- asks mountconfStaticDevs
  mapM_ (`mountMaybe` unmountFlag) $ configToTree static <$> M.lookup alias static

mkGroup :: [(Header, ProtoAction [String])] -> Maybe (RofiGroup MountConf)
mkGroup [] = Nothing
mkGroup as = let ((h, _):_) = as in
  Just $ titledGroup (show h) $ toRofiActions $ alignEntries $ fmap snd as

alignSep :: String
alignSep = " | "

alignEntries :: [ProtoAction [String]] -> [(String, RofiMountIO ())]
alignEntries ps = zip (align es) as
  where
    (es, as) = unzip $ fmap (\(ProtoAction e a) -> (e, a)) ps
    align = fmap (intercalate alignSep)
      . transpose
      . mapToLast pad
      . transpose
    pad xs = let m = getMax xs in fmap (\x -> take m (x ++ repeat ' ')) xs
    getMax = maximum . fmap length
    mapToLast _ []     = []
    mapToLast _ [x]    = [x]
    mapToLast f (x:xs) = f x : mapToLast f xs

--------------------------------------------------------------------------------
-- | Global config used in the reader monad stack

data MountConf = MountConf
  { mountconfVolatilePath :: FilePath
  , mountconfRofiArgs     :: [String]
  , mountconfStaticDevs   :: M.Map String TreeConfig
  , mountconfVerbose      :: Bool
  } deriving Show

instance RofiConf MountConf where
  defArgs MountConf { mountconfRofiArgs = a } = a

--------------------------------------------------------------------------------
-- | Mountable typeclass
--
-- Class to provide common interface for anything that can be mounted.

class Mountable a where
  -- | Mount the given type (or dismount if False is passed)
  mount :: a -> Bool -> RofiMountIO MountResult

  mountMaybe :: a -> Bool -> RofiMountIO ()
  mountMaybe dev mountFlag = do
    mounted <- isMounted dev
    verbose <- asks mountconfVerbose
    if mountFlag == mounted
      then (io . notifyMountResult mounted (getLabel dev)) =<< mount dev mountFlag
      else when verbose notify'
    where
      notify' = io $ notify IconInfo (getLabel dev ++ " already mounted") Nothing

  -- | Check if the mounting utilities are present
  allInstalled :: a -> RofiMountIO Bool

  -- | Return a string representing the label of the device
  getLabel :: a -> String

  -- | Determine if the given type is mounted or not
  isMounted :: a -> RofiMountIO Bool

--------------------------------------------------------------------------------
-- | Actionable typeclass
--
-- Class to provide common interface for anything that can be presented in the
-- Rofi menu as an action. Note that this must be separate from the Mountable
-- class above because some devices are represented as trees, and displaying
-- these trees in the rofi menu only requires that the tree itself be presented
-- and not its subcomponents.

class Mountable a => Actionable a where
  -- | Return a string to go in the Rofi menu for the given type
  fmtEntry :: a -> [String]
  fmtEntry d = [getLabel d]

  groupHeader :: a -> Header

  -- | Given a mountable type, return a rofi action (string to go in the
  -- Rofi prompt and an action to perform when it is selected)
  mkAction :: a -> RofiMountIO (Header, ProtoAction [String])
  mkAction dev = do
    m <- isMounted dev
    i <- allInstalled dev
    let h = groupHeader dev
    let action = when i $ mountMaybe dev m
    let entry = case fmtEntry dev of
              (e:es) -> (mountedPrefix m i ++ e):es
              _      -> []
    return (h, ProtoAction entry action)
    where
      mountedPrefix False True = "  "
      mountedPrefix True True  = "* "
      mountedPrefix _ False    = "! "

mountableToAction :: Actionable a => RofiMountIO [a] -> RofiMountIO [(Header, ProtoAction [String])]
mountableToAction ms = mapM mkAction =<< ms

type RofiMountIO a = RofiIO MountConf a

-- headers appear in the order listed here (per Enum)
data Header = CIFSHeader
  | SSHFSHeader
  | VeracryptHeader
  | RemovableHeader
  | MTPFSHeader
  deriving (Enum, Eq)

instance Show Header where
  show h = case h of
    CIFSHeader      -> suffix "CIFS"
    SSHFSHeader     -> suffix "SSHFS"
    VeracryptHeader -> suffix "Veracrypt"
    RemovableHeader -> suffix "Removable"
    MTPFSHeader     -> suffix "MTPFS"
    where
      suffix = (++ " Devices")

instance Ord Header where
  compare x y = compare (fromEnum x) (fromEnum y)

data ProtoAction a = ProtoAction a (RofiMountIO ())

--------------------------------------------------------------------------------
-- | Static device configuration
--
-- Static devices are defined in a YAML file. These types/instances describe how
-- to parse said YAML file.

defaultTries :: Integer
defaultTries = 2

(.:&) :: FromJSON a => Object -> T.Text -> Parser (V.Vector a)
(.:&) o t = o .:? t .!= V.empty

data MountConfig = MountConfig
  { mountMountpoint :: FilePath
  , mountLabel :: Maybe String
  } deriving Show

instance FromJSON MountConfig where
  parseJSON = withObject "mount" $ \o -> MountConfig
    <$> o .: "mountpoint"
    <*> o .:? "label"

data BitwardenConfig = BitwardenConfig
  { bitwardenKey :: String
  , bitwardenTries :: Integer }
  deriving Show

instance FromJSON BitwardenConfig where
  parseJSON = withObject "bitwarden" $ \o -> BitwardenConfig
    <$> o .: "key"
    <*> o .:? "tries" .!= defaultTries

newtype LibSecretConfig = LibSecretConfig
  { libsecretAttributes :: M.Map String String }
  deriving Show

instance FromJSON LibSecretConfig where
  parseJSON = withObject "libsecret" $ \o -> LibSecretConfig
    <$> o .: "attributes"

newtype PromptConfig = PromptConfig
  { promptTries :: Integer }
  deriving Show

instance FromJSON PromptConfig where
  parseJSON = withObject "libsecret" $ \o -> PromptConfig
    <$> o .: "tries" .!= defaultTries

data PasswordConfig = PasswordConfig
  { passwordBitwarden :: Maybe BitwardenConfig
  , passwordLibSecret :: Maybe LibSecretConfig
  , passwordPrompt    :: Maybe PromptConfig
  }
  deriving Show

instance FromJSON PasswordConfig where
  parseJSON = withObject "password" $ \o -> PasswordConfig
    <$> o .:? "bitwarden"
    <*> o .:? "libsecret"
    <*> o .:? "prompt"

data CIFSOptsConfig = CIFSOptsConfig
  { cifsoptsUsername   :: Maybe String
  , cifsoptsWorkgroup  :: Maybe String
  , cifsoptsUID        :: Maybe Integer
  , cifsoptsGID        :: Maybe Integer
  , cifsoptsIocharset :: Maybe String
  } deriving Show

instance FromJSON CIFSOptsConfig where
  parseJSON = withObject "options" $ \o -> CIFSOptsConfig
    <$> o .:? "username"
    <*> o .:? "workgroup"
    <*> o .:? "uid"
    <*> o .:? "gid"
    <*> o .:? "isocharset"

data DataConfig = VeracryptConfig
  { veracryptVolume   :: String
  , veracryptPassword :: Maybe PasswordConfig
  } | SSHFSConfig
  { sshfsRemote :: String
  } | CIFSConfig
  { cifsRemote     :: String
  , cifsSudo       :: Bool
  , cifsPassword   :: Maybe PasswordConfig
  , cifsOpts       :: Maybe CIFSOptsConfig
  } deriving Show

data DeviceConfig = DeviceConfig
  { deviceMount :: MountConfig
  , deviceData  :: DataConfig
  } deriving Show

data TreeConfig = TreeConfig
  { treeParent         :: DeviceConfig
  , treeconfigChildren :: V.Vector String
  } deriving Show

instance FromJSON TreeConfig where
  parseJSON = withObject "devices" $ \o -> do
    devType <- o .: "type"
    deps <- o .:& "depends"
    mountconf <- o .: "mount"
    devData <- case (devType :: String) of
                 "cifs"      -> CIFSConfig
                   <$> o .: "remote"
                   <*> o .:? "sudo" .!= False
                   <*> o .:? "password"
                   <*> o .:? "options"
                 "sshfs"     -> SSHFSConfig
                   <$> o .: "remote"
                 "veracrypt" -> VeracryptConfig
                   <$> o .: "volume"
                   <*> o .:? "password"
                 -- TODO make this skip adding an entry to the map rather than
                 -- skipping the map entirely
                 _           -> fail $ "unknown device type: " ++ devType
    return $ TreeConfig
      { treeParent = DeviceConfig
                      { deviceMount = mountconf
                      , deviceData = devData
                      }
      , treeconfigChildren = deps
      }

data StaticConfig = StaticConfig
  { staticconfigTmpPath :: Maybe String
  , staticconfigVerbose :: Maybe Bool
  , staticconfigDevices :: M.Map String TreeConfig
  } deriving Show

instance FromJSON StaticConfig where
  parseJSON = withObject "devices" $ \o -> StaticConfig
    <$> o .:? "mountdir"
    <*> o .:? "verbose"
    <*> o .: "devices"

--------------------------------------------------------------------------------
-- | Static devices trees
--
-- Static devices as defined in the config file may declare dependencies on
-- other static devices, and thus are best represented as a tree. Note that the
-- tree is both Actionable and Mountable, where each node in the tree is only
-- Mountable; this is because trees need to be displayed and chosen in the Rofi
-- menu.

data Tree a = Tree a [Tree a] deriving (Eq, Show)

type StaticConfigTree = Tree DeviceConfig

instance Mountable a => Mountable (Tree a) where
  mount (Tree p cs) False = mapM_ (`mountMaybe` False) cs >> mount p False
  mount (Tree p _) True = mount p True

  isMounted (Tree p _) = isMounted p

  allInstalled (Tree p cs) = do
    res <- and <$> mapM allInstalled cs
    if res then allInstalled p else return res

  getLabel (Tree p _) = getLabel p

instance Actionable (Tree DeviceConfig) where
  fmtEntry (Tree p@DeviceConfig{ deviceData = d } _) = [getLabel p, target d] 
    where
      target CIFSConfig{ cifsRemote = r } = r
      target SSHFSConfig{ sshfsRemote = r } = r
      target VeracryptConfig{ veracryptVolume = v } = v

  groupHeader (Tree DeviceConfig{ deviceData = d } _) =
    case d of
      CIFSConfig{} -> CIFSHeader
      SSHFSConfig{} -> SSHFSHeader
      VeracryptConfig{} -> VeracryptHeader

configToTree' :: M.Map String TreeConfig -> [StaticConfigTree]
configToTree' devMap = configToTree devMap <$> M.elems devMap

configToTree :: M.Map String TreeConfig -> TreeConfig -> StaticConfigTree
configToTree devMap TreeConfig{ treeParent = p, treeconfigChildren = c } =
  Tree p $ fmap go V.toList c
  where
    go ds = configToTree devMap <$> mapMaybe (`M.lookup` devMap) ds

--------------------------------------------------------------------------------
-- | Static devices
--
-- This is complex because there may be multiple classes of static devices
-- in the config file, and each device may depend on another device that is
-- a different class (eg sshfs on cifs). I deal with this by abstracting the
-- differences between each class in a sum-record type; in this way the
-- processing "splits" and "converges" entirely in this function, so nothing
-- outside of these needs to be aware of these different classes.

instance Mountable DeviceConfig where
  mount DeviceConfig{ deviceMount = m, deviceData = devData} False = do
    m' <- getAbsMountpoint m
    withTmpMountDir m'
      $ io
      $ case devData of
          SSHFSConfig{ sshfsRemote = r } -> mountSSHFS m' r
          CIFSConfig
            { cifsRemote = r
            , cifsSudo = s
            , cifsPassword = p
            , cifsOpts = o
            } -> mountCIFS s r m' o p
          VeracryptConfig{ veracryptPassword = p, veracryptVolume = v } ->
            mountVeracrypt m' p v

  mount DeviceConfig{ deviceMount = m, deviceData = d } True = do
    m' <- getAbsMountpoint m
    runAndRemoveDir m' $ io $ case d of
      CIFSConfig{ cifsSudo = s } -> runMountSudoMaybe s "umount" [m'] 
      VeracryptConfig{}          -> runVeraCrypt ["-d", m'] ""
      _                          -> runMount "umount" [m'] ""

  allInstalled DeviceConfig{ deviceData = devData } = io $ isJust
    <$> findExecutable (exe devData)
    where
      exe SSHFSConfig{} = "sshfs"
      exe CIFSConfig{} = "mount.cifs"
      exe VeracryptConfig{} = "veracrypt"

  isMounted DeviceConfig{ deviceMount = m } =
    (io . isDirMounted) =<< getAbsMountpoint m

  getLabel DeviceConfig
    { deviceMount = MountConfig { mountMountpoint = p, mountLabel = l }
    } = fromMaybe (takeFileName p) l

mountSSHFS :: FilePath -> String -> IO MountResult
mountSSHFS mountpoint remote = runMount "sshfs" [remote, mountpoint] ""

mountCIFS :: Bool -> String -> FilePath -> Maybe CIFSOptsConfig 
  -> Maybe PasswordConfig -> IO MountResult
mountCIFS useSudo remote mountpoint opts pwdConfig =
  withPasswordGetter pwdConfig runPwd run
  where
    run = runMountSudoMaybe useSudo "mount.cifs" args
    runPwd p = runMountSudoMaybe' useSudo "mount.cifs" args [("PASSWD", p)]
    args = [remote, mountpoint] ++ maybe [] (\o -> ["-o", fromCIFSOpts o]) opts

fromCIFSOpts :: CIFSOptsConfig -> String
fromCIFSOpts o = intercalate "," $ mapMaybe concatMaybe fs
  where
    fs = [ ("username", cifsoptsUsername)
         , ("workgroup", cifsoptsWorkgroup)
         , ("uid", fmap show . cifsoptsUID)
         , ("gid", fmap show .  cifsoptsGID)
         , ("iocharset", cifsoptsIocharset)
         ]
    concatMaybe (k, f) = (\v -> k ++ "=" ++ v) <$> f o

mountVeracrypt :: FilePath -> Maybe PasswordConfig -> String -> IO MountResult
mountVeracrypt mountpoint pwdConfig volume = 
  withPasswordGetter pwdConfig (runVeraCrypt (args ++ ["--stdin"]))
  $ runVeraCrypt args ""
  where
    args = [volume, mountpoint]

-- NOTE: the user is assumed to have added themselves to the sudoers file so
-- that this command will work
runVeraCrypt :: [String] -> String -> IO MountResult
runVeraCrypt args = runMount "sudo" (defaultArgs ++ args)
  where
    defaultArgs = ["/usr/bin/veracrypt", "--text", "--non-interactive"]

getAbsMountpoint :: MountConfig -> RofiMountIO FilePath
getAbsMountpoint MountConfig{ mountMountpoint = m } =
  asks $ flip appendRoot m . mountconfVolatilePath

getStaticActions :: RofiMountIO [(Header, ProtoAction [String])]
getStaticActions = mountableToAction $ asks $ configToTree' . mountconfStaticDevs 

--------------------------------------------------------------------------------
-- | Password-getting functions for static devices

type PasswordGetter = IO (Maybe String)

runSecret :: M.Map String String -> PasswordGetter
runSecret kvs = readCmdSuccess "secret-tool" ("lookup":kvs') ""
  where
    kvs' = concatMap (\(k, v) -> [k, v]) $ M.toList kvs 

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
configToPwd PasswordConfig
  { passwordBitwarden = b
  , passwordLibSecret = s
  , passwordPrompt = p
  } =
    getBW b `runMaybe` getLS s `runMaybe` getPrompt p
  where
    getBW (Just BitwardenConfig{ bitwardenKey = k, bitwardenTries = n }) =
      runPromptLoop n $ runBitwarden k
    getBW _ = return Nothing
    getLS = maybe (return Nothing) (runSecret . libsecretAttributes)
    getPrompt = maybe (return Nothing) (flip runPromptLoop readPassword . promptTries)
    runMaybe x y = (\r -> if isNothing r then y else return r) =<< x

withPasswordGetter :: Maybe PasswordConfig -> (String -> IO MountResult)
  -> IO MountResult -> IO MountResult
withPasswordGetter (Just pwdConfig) runPwd _ =
  maybe (return $ MountError "Password could not be obtained") runPwd
  =<< configToPwd pwdConfig
withPasswordGetter Nothing _ run = run

--------------------------------------------------------------------------------
-- | Removable devices
--
-- A device which can be removed (such as a flash drive). These are distinct
-- from any device in the static configuration in that they only have device
-- addresses (eg in /dev) and labels.

data Removable = Removable
    { removablePath  :: String
    , removableLabel :: String
    }
    deriving (Eq, Show)

instance Mountable Removable where
  mount Removable { removablePath = d } m =
    io $ runMount "udisksctl" [c, "-b", d] ""
    where
      c = if m then "unmount" else "mount"

  allInstalled _ = fmap isJust $ io $ findExecutable "udisksctl"

  isMounted Removable { removablePath = d } = elem d <$> io curDeviceSpecs

  getLabel Removable { removableLabel = l } = l

instance Actionable Removable where
  fmtEntry Removable { removablePath = d, removableLabel = l } = [l, d]

  groupHeader _ = RemovableHeader

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
    mk d l = Just $ Removable { removablePath = d, removableLabel = l }

getRemovableActions :: RofiMountIO [(Header, ProtoAction [String])]
getRemovableActions = mountableToAction getRemovableDevices

--------------------------------------------------------------------------------
-- | MTP devices

data MTPFS = MTPFS
    { mtpfsBus         :: String
    , mtpfsDevice      :: String
    , mtpfsMountpoint  :: FilePath
    , mtpfsDescription :: String
    }
    deriving (Eq, Show)

instance Mountable MTPFS where
  mount MTPFS { mtpfsBus = b, mtpfsDevice = n, mtpfsMountpoint = m } False = do
    -- TODO add autodismount to options
    let dev = "-device=" ++ b ++ "," ++ n
    withTmpMountDir m $ io $ runMount "jmtpfs" [dev, m] ""

  mount MTPFS { mtpfsMountpoint = m } True =
    runAndRemoveDir m $ io $ runMount "umount" [m] ""

  -- | return True always since the list won't even show without jmtpfs
  allInstalled _ = return True

  isMounted = io . isDirMounted <$> mtpfsMountpoint

  getLabel = mtpfsDescription

-- | Return list of all available MTP devices
getMTPDevices :: RofiMountIO [MTPFS]
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
           { mtpfsBus = busNum
           , mtpfsDevice = devNum
           , mtpfsMountpoint = dir </> canonicalize d
           , mtpfsDescription = d
           }
      _ -> Nothing
    canonicalize = mapMaybe repl
    repl c
      | c `elem` ("\"*/:<>?\\|" :: String) = Nothing
      | c == ' ' = Just '-'
      | otherwise = Just c

getMTPActions :: RofiMountIO [(Header, ProtoAction [String])]
getMTPActions = mountableToAction getMTPDevices

instance Actionable MTPFS where
  fmtEntry d = [getLabel d]

  groupHeader _ = MTPFSHeader

--------------------------------------------------------------------------------
-- | Notifications

data NotifyIcon = IconError | IconInfo

instance Show NotifyIcon where
  show IconError = "dialog-error-symbolic"
  show IconInfo = "dialog-information-symbolic"

notifyMountResult :: Bool -> String -> MountResult -> IO ()
notifyMountResult mounted label result = case result of
  MountError e -> notify IconError (printf "Failed to %s %s" verb label) $ Just e
  MountSuccess -> notify IconInfo (printf "Successfully %sed %s" verb label) Nothing
  where
    verb = if mounted then "unmount" else "mount" :: String

notify :: NotifyIcon -> String -> Maybe String -> IO ()
notify icon summary body = void $ spawnProcess "notify-send"
  $ maybe args (\b -> args ++ [b]) body
  where
    args = ["-i", show icon, summary]

--------------------------------------------------------------------------------
-- | Mount commands

data MountResult = MountSuccess | MountError String deriving (Show, Eq)

runMount :: String -> [String] -> String -> IO MountResult
runMount cmd args stdin = eitherToMountResult <$> readCmdEither cmd args stdin

runMount' :: String -> [String] -> String -> [(String, String)] -> IO MountResult
runMount' cmd args stdin environ = eitherToMountResult
  <$> readCmdEither' cmd args stdin environ

runMountSudoMaybe :: Bool -> String -> [String] -> IO MountResult
runMountSudoMaybe useSudo cmd args =
  runMountSudoMaybe' useSudo cmd args []

runMountSudoMaybe' :: Bool -> String -> [String] -> [(String, String)] -> IO MountResult
runMountSudoMaybe' useSudo cmd args environ = maybe
  (runMount' cmd args "" environ)
  (\r -> runSudoMount' r cmd args environ)
  =<< if useSudo then readPassword' "Sudo Password" else return Nothing

-- TODO untested
-- runSudoMount :: String -> String -> [String] -> String -> IO MountResult
-- runSudoMount rootpass cmd args stdin = runSudoMount' rootpass cmd args stdin []

runSudoMount' :: String -> String -> [String] -> [(String, String)] -> IO MountResult
runSudoMount' rootpass cmd args environ = runMount "sudo" args' rootpass
  where
    args' = ["-S"] ++ environ' ++ [cmd] ++ args
    environ' = fmap (\(k, v) -> k ++ "=" ++ v) environ

eitherToMountResult :: Either (Int, String, String) String -> MountResult
eitherToMountResult (Right _) = MountSuccess
eitherToMountResult (Left (_, _, e)) = MountError e 

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

rmDirOnMountError :: FilePath -> RofiMountIO MountResult -> RofiMountIO MountResult
rmDirOnMountError d f = do
  res <- f
  unless (res == MountSuccess) $ rmDirMaybe d
  return res

-- | Run a mount command and create the mountpoint if it does not exist, and
-- remove the mountpoint if a mount error occurs
withTmpMountDir :: FilePath -> RofiMountIO MountResult -> RofiMountIO MountResult
withTmpMountDir m = rmDirOnMountError m
  . bracketOnError_ (mkDirMaybe m) (rmDirMaybe m)

-- | Run an unmount command and remove the mountpoint if no errors occur
runAndRemoveDir :: FilePath -> RofiMountIO MountResult -> RofiMountIO MountResult
runAndRemoveDir m f = do
  res <- catch f (return . MountError . (displayException :: SomeException -> String))
  when (res == MountSuccess) $ rmDirMaybe m
  return res

mkDirMaybe :: FilePath -> RofiMountIO ()
mkDirMaybe fp = whenInMountDir fp $ io $ createDirectoryIfMissing True fp

rmDirMaybe :: FilePath -> RofiMountIO ()
rmDirMaybe fp = whenInMountDir fp $ unlessMountpoint fp
  $ asks mountconfVolatilePath >>= io . rmUntil fp
  where
    rmUntil cur target = unless (target == cur) $ do
      removePathForcibly cur
      rmUntil (takeDirectory cur) target

whenInMountDir :: FilePath -> RofiMountIO () -> RofiMountIO ()
whenInMountDir fp f = do
  mDir <- asks mountconfVolatilePath
  when (mDir `isPrefixOf` fp) f

unlessMountpoint :: MonadIO m => FilePath -> m () -> m ()
unlessMountpoint fp f = do
  mounted <- io $ isDirMounted fp
  unless mounted f

isDirMounted :: FilePath -> IO Bool
isDirMounted fp = elem fp <$> curMountpoints

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
