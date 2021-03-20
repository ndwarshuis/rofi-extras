{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns   #-}

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
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Yaml

import           GHC.Generics()

import           Rofi.Command

import           Text.Printf

import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit (ExitCode(..))
import           System.FilePath.Posix
import           System.Posix.User     (getEffectiveUserName)
import           System.Process

import           UnliftIO.Exception

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse args = case getOpt Permute options args of
  -- (o, n, [])   -> initMountConf n >>= \i -> runMounts $ foldl (flip id) i o
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

data DeviceConfig = VeracryptConfig
  { _veracryptMount :: MountConfig
  , _veracryptVolume :: String
  , _veracryptDepends :: V.Vector String
  , _veracryptPassword :: Maybe PasswordConfig
  } | SSHFSConfig
  { _sshfsMount :: MountConfig
  , _sshfsRemote :: String
  , _sshfsDepends :: V.Vector String
  } | CIFSConfig
  { _cifsMount :: MountConfig
  , _cifsRemote :: String
  , _cifsDepends :: V.Vector String
  , _cifsPassword :: Maybe PasswordConfig
  } deriving Show

instance FromJSON DeviceConfig where
  parseJSON = withObject "devices" $ \o -> do
    devType <- o .: "type"
    case (devType :: String) of
      "cifs"      -> CIFSConfig
        <$> o .: "mount"
        <*> o .: "remote"
        <*> o .:& "depends"
        <*> o .:? "password"
      "sshfs"     -> SSHFSConfig
        <$> o .: "mount"
        <*> o .: "remote"
        <*> o .:& "depends"
      "veracrypt" -> VeracryptConfig
        <$> o .: "mount"
        <*> o .: "volume"
        <*> o .:& "depends"
        <*> o .:? "password"
      _           -> fail "unknown device type"

data StaticConfig = StaticConfig
  { _staticconfigTmpPath :: Maybe String
  , _staticconfigDevices :: M.Map String DeviceConfig
  } deriving Show

instance FromJSON StaticConfig where
  parseJSON = withObject "devices" $ \o -> StaticConfig
    <$> o .:? "mountdir"
    <*> o .: "devices"

--------------------------------------------------------------------------------
-- | Static Devices typeclass
--
-- A class to represent devices defined in the static configuration (eg the YAML
-- file). Its methods define the machinery to extract specific devies types
-- from the parse tree.

fromConfig :: M.Map String DeviceConfig -> RofiIO MountConf [DevTriple]
fromConfig st = do
  p <- asks mountconfVolatilePath
  mapM (configToDev p st) $ M.elems st

foldTriples :: [Triple a b c] -> ([a], [b], [c])
foldTriples = foldl stackTriples ([], [], [])

stackTriples :: ([a], [b], [c]) -> Triple a b c -> ([a], [b], [c])
stackTriples (c, v, s) (First x)  = (x:c, v, s)
stackTriples (c, v, s) (Second x) = (c, x:v, s)
stackTriples (c, v, s) (Third x) = (c, v, x:s)

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
    , mountconfStaticDevs   :: M.Map String DeviceConfig
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
  sysd <- io getSystemdDevices
  (cifsDevs, sshfsDevs, vcDevs) <- foldTriples
    <$> (fromConfig =<< asks mountconfStaticDevs)
  sequence
    [ mkGroup2 "SSHFS Devices" (filterSysd SystemdSSHFS sysd) sshfsDevs
    , mkGroup "CIFS Devices" cifsDevs
    , mkGroup2 "Veracrypt Devices" (filterSysd SystemdVeracrypt sysd) vcDevs
    , mkGroup "Removable Devices"  =<< getRemovableDevices
    , mkGroup "MTP Devices" =<< getMTPDevices
    ]
  where
    filterSysd t = filter (\s -> sysdType s == t)

mountByAlias :: Bool -> String -> RofiIO MountConf ()
mountByAlias unmountFlag alias = do
  static <- asks mountconfStaticDevs
  volatilePath <- asks mountconfVolatilePath
  forM_(M.lookup alias static) $ \d -> do
    res <- configToDev volatilePath static d
    case res of
      First d'  -> mount' d'
      Second d' -> mount' d'
      Third d'  -> mount' d'
  where
    mount' :: Mountable a => a -> RofiIO MountConf ()
    mount' = flip mount unmountFlag

mkGroup :: Mountable d => String -> [d] -> RofiIO MountConf (RofiGroup MountConf)
mkGroup header devs = sortGroup header <$> mapM mkAction devs

mkGroup2 :: (Mountable d, Mountable e) => String
  -> [d] -> [e] -> RofiIO MountConf (RofiGroup MountConf)
mkGroup2 header devs1 devs2 = do
  r1 <- mapM mkAction devs1
  r2 <- mapM mkAction devs2
  return $ sortGroup header (r1 ++ r2)

sortGroup :: String -> [(String, RofiIO MountConf ())] -> RofiGroup MountConf
sortGroup header = titledGroup header . alignEntries . toRofiActions

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

data CIFS = CIFS Removable FilePath (Maybe PasswordGetter) Dependency

instance Show CIFS where
  show (CIFS r f _ d) = unwords [show r, show f, "<Pwd>", show d]

instance Mountable CIFS where
  mount (CIFS Removable{ label = l } m getPwd deps) False = do
    mountDependencies deps
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

  mount (CIFS Removable{ label = l } m _ _) True = umountNotify l m

  allInstalled _ = io $ isJust <$> findExecutable "mount.cifs"

  isMounted (CIFS _ dir _ _) = io $ isDirMounted dir

  fmtEntry (CIFS r _ _ _) = fmtEntry r

--------------------------------------------------------------------------------
-- | SSHFS Devices
--
-- This wraps the Removable device (since it is removable) and also adds its
-- own mount options. If the path does not point to an aliased entry in the ssh
-- config that specifies the port, hostname, user, and identity file, these
-- need to be passed as mount options.

data SSHFS = SSHFS Removable FilePath Dependency deriving Show

instance Mountable SSHFS where
  mount (SSHFS Removable{ deviceSpec = d, label = l } m deps) False = do
    mountDependencies deps
    bracketOnError_
      (mkDirMaybe m)
      (rmDirMaybe m)
      (io $ runMountNotify "sshfs" [d, m] l False)

  mount (SSHFS Removable{ label = l } m _) True = umountNotify l m

  allInstalled _ = fmap isJust $ io $ findExecutable "sshfs"

  isMounted (SSHFS _ dir _) = io $ isDirMounted dir

  fmtEntry (SSHFS r _ _) = fmtEntry r

--------------------------------------------------------------------------------
-- | VeraCrypt Devices
--

data VeraCrypt = VeraCrypt Removable FilePath (Maybe PasswordGetter) Dependency

instance Show VeraCrypt where
  show (VeraCrypt r f _ d) = unwords [show r, show f, show d]

instance Mountable VeraCrypt where
  mount (VeraCrypt Removable{ deviceSpec = s, label = l } m getPwd deps) False = do
    mountDependencies deps
    bracketOnError_ (mkDirMaybe m) (rmDirMaybe m) mountMaybe
    where
      mountMaybe = io $ maybe (runVeraCryptWith "" []) (runVeraCryptWithPwd =<<) getPwd
      runVeraCryptWithPwd = maybe notifyFail (\p -> runVeraCryptWith p ["--stdin"])
      runVeraCryptWith stdin args = (\res -> notifyMounted (isRight res) False l)
        =<< runVeraCrypt stdin ([s, m] ++ args)
      notifyFail = notify "dialog-error-symbolic" $
        printf "Failed to get volume password for %s" l

  mount (VeraCrypt Removable{ label = l } m _ _) True = io $ do
    res <- runVeraCrypt "" ["-d", m]
    notifyMounted (isRight res) True l

  allInstalled _ = io $ isJust <$> findExecutable "veracrypt"

  isMounted (VeraCrypt _ dir _ _) = io $ isDirMounted dir

  fmtEntry (VeraCrypt r _ _ _) = fmtEntry r

-- NOTE: the user is assumed to have added themselves to the sudoers file so
-- that this command will work
runVeraCrypt :: String -> [String] -> IO (Either (Int, String, String) String)
runVeraCrypt stdin args = do
  readCmdEither "sudo" (defaultArgs ++ args) stdin
  where
    defaultArgs = ["/usr/bin/veracrypt", "--text", "--non-interactive"]

data Triple a b c = First a | Second b | Third c deriving Show

type DevTriple = Triple CIFS SSHFS VeraCrypt

-- TODO abstract parts of this away in new typeclass for static devices
configToDev :: FilePath -> M.Map String DeviceConfig -> DeviceConfig
  -> RofiIO MountConf DevTriple
configToDev v s CIFSConfig { _cifsMount = MountConfig { _mountMountPoint = m }
                           , _cifsRemote = t
                           , _cifsDepends = d
                           , _cifsPassword = p } = do
  -- stuff like this is totally refactorable
  let r = Removable { deviceSpec = smartSlashPrefix t, label = takeFileName m }
  d' <- getDependencies s $ V.toList d
  return $ First $ CIFS r (appendRoot v m) (configToPwd <$> p) d'
  where
    smartSlashPrefix a = if "//" `isPrefixOf` a then a else "//" ++ a
configToDev v s SSHFSConfig { _sshfsMount = MountConfig { _mountMountPoint = m }
                            , _sshfsDepends = d
                            , _sshfsRemote = t } = do
  let r = Removable { deviceSpec = t, label = takeFileName m }
  d' <- getDependencies s $ V.toList d
  return $ Second $ SSHFS r (appendRoot v m) d'
configToDev v s VeracryptConfig { _veracryptMount = MountConfig { _mountMountPoint = m }
                                , _veracryptVolume = t
                                , _veracryptDepends = d
                                , _veracryptPassword = p } = do
  let r = Removable { deviceSpec = t, label = takeFileName m }
  d' <- getDependencies s $ V.toList d
  return $ Third $ VeraCrypt r (appendRoot v m) (configToPwd <$> p) d'

--------------------------------------------------------------------------------
-- | Dependencies
--
-- Define a data structure that allows one device to depend on another. Since
-- each device is different and has a different typeclass instance, need to
-- include slots for all possible devices. For now only deal with static
-- devices.

data Dependency = Dependency
  { dependencySSHFS :: [SSHFS]
  , dependencyCIFS :: [CIFS]
  , dependencyVeracrypt :: [VeraCrypt]
  } deriving Show

getDependencies :: M.Map String DeviceConfig -> [String] -> RofiIO MountConf Dependency
getDependencies devMap aliases = do
  (c, s, v) <- fmap foldTriples
    $ fromConfig $ M.filterWithKey (\k _ -> k `elem` aliases) devMap
  return Dependency { dependencyCIFS = c
                    , dependencySSHFS = s
                    , dependencyVeracrypt = v}

mountDependencies :: Dependency -> RofiIO MountConf ()
mountDependencies Dependency { dependencyCIFS = c
                             , dependencySSHFS = s
                             , dependencyVeracrypt = v
                             } =
  mountAll c >> mountAll s >> mountAll v
  where
    mountAll :: Mountable a => [a] -> RofiIO MountConf ()
    mountAll = mapM_ (\d -> isMounted d >>= (\r -> unless r $ mount d False))

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
-- | Systemd typeclass

data SystemdMountType = SystemdVeracrypt | SystemdSSHFS deriving (Eq, Show)
  
data Systemd = Systemd
    { sysdType     :: SystemdMountType
    , sysdInstance :: String
    }
    deriving (Eq, Show)

instance Mountable Systemd where
  mount s@Systemd { sysdInstance = i } m = let
    unit = fmtSysdInstanceName s
    operation = if m then "stop" else "start" in
    io $ runMountNotify "systemctl" ["--user", operation, unit] i m

  allInstalled Systemd { sysdType = SystemdVeracrypt } =
    io $ isJust <$> findExecutable "veracrypt"

  allInstalled Systemd { sysdType = SystemdSSHFS } =
    io $ isJust <$> findExecutable "sshfs"

  isMounted s = let
    unit = fmtSysdInstanceName s
    args = ["--user", "is-active", "--quiet", unit] in
    io $ (\(ec, _, _) -> ec == ExitSuccess)
    <$> readProcessWithExitCode "systemctl" args ""

  fmtEntry Systemd { sysdInstance = i } = i ++ alignSepPre ++ "Systemd"

fmtSysdInstanceName :: Systemd -> String
fmtSysdInstanceName Systemd { sysdType = SystemdVeracrypt, sysdInstance = i } =
  "mount-veracrypt@" ++ i ++ ".service"
fmtSysdInstanceName Systemd { sysdType = SystemdSSHFS, sysdInstance = i } =
  "mount-sshfs@" ++ i ++ ".service"

getSystemdDevices :: IO [Systemd]
getSystemdDevices = do
  systemdHome <- io $ getXdgDirectory XdgConfig "systemd/user"
  io $ mapMaybe toDev
    <$> (filterM (doesDirectoryExist . (systemdHome </>))
    =<< listDirectory systemdHome)
  where
    toDev (splitInstance "mount-veracrypt@" -> Just s) =
      Just $ Systemd { sysdType = SystemdVeracrypt , sysdInstance = s }
    toDev (splitInstance "mount-sshfs@" -> Just s) =
      Just $ Systemd { sysdType = SystemdSSHFS , sysdInstance = s }
    toDev _ = Nothing
    splitInstance p = fmap (takeWhile (not . (==) '.')) . stripPrefix p

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
