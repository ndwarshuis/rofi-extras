{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

import           Control.Concurrent
import           Control.Monad

import           Data.Aeson
import           Data.Maybe
import           Data.String
import           Data.UnixTime

import           DBus
import           DBus.Client

import           GHC.Generics

import           Rofi.Command

import           Text.Read

import           System.Clipboard
import           System.Directory
import           System.Environment
import           System.Exit
import           System.Process

main :: IO ()
main = runChecks >> getArgs >>= parse

parse :: [String] -> IO ()
parse ["-d", t] = case readMaybe t of { Just t' -> runDaemon t'; _ -> usage }
parse ("-c":args) = runClient args
parse _         = usage

usage :: IO ()
usage = putStrLn $ joinNewline
  [ "daemon mode: rofi-bw -d TIMEOUT"
  , "client mode: rofi-bw -c [ROFI-ARGS]"
  ]

runChecks :: IO ()
runChecks = checkExe "bw" >> checkExe "rofi"

checkExe :: String -> IO ()
checkExe cmd = do
  res <- findExecutable cmd
  unless (isJust res) $ do
    putStrLn $ "Could not find executable: " ++ cmd
    exitWith $ ExitFailure 1

--------------------------------------------------------------------------------
-- | Daemon
--
-- Daemon will export an interface on DBus with two methods:
-- * get current session id -  if no session is active, launch Rofi to prompt
--   for a password; return session id or null if password is invalid
-- * lock session - destroy the current session id if active
--
-- The session ID will be valid only as long as TIMEOUT

newtype BWConf = BWConf
    { timeout :: UnixDiffTime
    }

data CurrentSession = CurrentSession
    { timestamp :: UnixTime
    , hash      :: String
    }

type Session = MVar (Maybe CurrentSession)

runDaemon :: Int -> IO ()
runDaemon t = do
  ses <- newMVar Nothing
  let c = BWConf { timeout = UnixDiffTime (fromIntegral t) 0 }
  startService c ses
  forever $ threadDelay 1000000

lockSession :: Session -> IO ()
lockSession ses = void $ swapMVar ses Nothing

getSession :: BWConf -> Session -> IO String
getSession BWConf { timeout = t } ses = do
  ut <- getUnixTime
  modifyMVar ses $ \s -> case s of
    Just CurrentSession { timestamp = ts, hash = h } ->
      if diffUnixTime ut ts > t then getNewSession else return (s, h)
    Nothing -> getNewSession
  where
    getNewSession = do
      pwd <- readPassword
      newHash <- join <$> mapM readSession pwd
      (, fromMaybe "" newHash) <$> mapM newSession newHash
    newSession h = do
      ut <- getUnixTime
      return CurrentSession { timestamp = ut, hash = h }

readPassword :: IO (Maybe String)
readPassword = readCmdSuccess "rofi" args ""
  where
    args = dmenuArgs ++ ["-p", "Password", "-password"]

readSession :: String -> IO (Maybe String)
readSession pwd = readCmdSuccess "bw" ["unlock", pwd, "--raw"] ""

--------------------------------------------------------------------------------
-- | Client
--
-- The client will get the current session from the daemon (if it can) and then
-- go through a decision-tree like selection process to retrieve information as
-- needed. This will be in the form of the following menus:
--
-- Main menus
-- - Lock Session -> lock the session
-- - Browse logins -> show new menu of all logins
--   - select an entry -> show new menu with entry contents
--     - All -> copy all to clipboard
--     - username (if applicable) -> copy to clipboard
--     - password (if applicable) -> copy to clipboard
--     - anything else (notes and such) -> copy to clipboard

runClient :: [String] -> IO ()
runClient a = do
  let c = RofiConf { defArgs = a }
  runRofiPrompt c $ selectAction $ emptyMenu
    { groups = [untitledGroup $ toRofiActions ras]
    , prompt = Just "Action"
    }
  where
    ras = [ ("Browse Logins", browseLogins)
          , ("Lock Session", io callLockSession)
          ]

browseLogins :: RofiPrompt ()
browseLogins = do
  session <- io callGetSession
  forM_ session $ getItems >=> selectItem

getItems :: String -> RofiPrompt [Item]
getItems session = do
  items <- io $ readProcess "bw" ["list", "items", "--session", session] ""
  return $ filter notEmpty $ fromMaybe [] $ decode $ fromString items
  where
    notEmpty Item { login = Login { username = Nothing, password = Nothing } }
      = False
    notEmpty _ = True

data Item = Item
    { name  :: String
    , login :: Login
    }
    deriving (Show)

instance FromJSON Item where
  parseJSON (Object o) = Item
    <$> o .: "name"
    <*> o .:? "login" .!= Login { username = Nothing, password = Nothing }
  parseJSON _ = mzero

data Login = Login
    { username :: Maybe String
    , password :: Maybe String
    }
    deriving (Show, Generic)

instance FromJSON Login

-- TODO make menu buttons here to go back and to copy without leaving
-- the current menu
selectItem :: [Item] -> RofiPrompt ()
selectItem items = selectAction $ emptyMenu
  { groups = [untitledGroup $ itemsToRofiActions items]
  , prompt = Just "Login"
  }

itemsToRofiActions :: [Item] -> RofiActions
itemsToRofiActions = toRofiActions . fmap (\i -> (name i, selectCopy $ login i))

selectCopy :: Login -> RofiPrompt ()
selectCopy l = selectAction $ emptyMenu
  { groups = [untitledGroup $ loginToRofiActions l copy]
  , prompt = Just "Copy"
  , hotkeys = [copyHotkey, backHotkey]
  }
  where
    copy = io . setClipboardString
    copyRepeat s = copy s >> selectCopy l
    copyHotkey = Hotkey
      { keyCombo = "Alt+c"
      , keyIndex = 1
      , keyDescription = "Copy One"
      , keyActions = loginToRofiActions l copyRepeat
      }
    backHotkey = Hotkey
      { keyCombo = "Alt+q"
      , keyIndex = 2
      , keyDescription = "Back"
      -- TODO this is overly complicated, all entries do the same thing
      -- TODO this is slow, we can cache the logins somehow...
      , keyActions = loginToRofiActions l (const browseLogins)
      }

loginToRofiActions :: Login -> (String -> RofiPrompt ()) -> RofiActions
loginToRofiActions Login { username = u, password = p } a =
  toRofiActions $ catMaybes [user, pwd]
  where
    copyIfJust f = fmap $ liftM2 (,) f a
    fmtUsername s = "Username (" ++ s ++ ")"
    fmtPassword s = "Password (" ++ take 32 (replicate (length s) '*') ++ ")"
    user = copyIfJust fmtUsername u
    pwd = copyIfJust fmtPassword p

--------------------------------------------------------------------------------
-- | DBus

busname :: BusName
busname = "org.rofi.bitwarden"

path :: ObjectPath
path = "/bitwarden"

interface :: InterfaceName
interface = "org.rofi.bitwarden.session"

memGetSession :: MemberName
memGetSession = "GetSession"

memLockSession :: MemberName
memLockSession = "LockSession"

startService :: BWConf -> Session -> IO ()
startService c ses = do
  client <- connectSession
  let flags = [nameAllowReplacement, nameReplaceExisting]
  _ <- requestName client busname flags
  putStrLn "Started rofi bitwarden dbus client"
  export client path defaultInterface
    { interfaceName = interface
    , interfaceMethods =
      [ autoMethod memGetSession $ getSession c ses
      , autoMethod memLockSession $ lockSession ses
      ]
    }

callLockSession :: IO ()
callLockSession = do
  reply <- callMethod $ methodCall path interface memLockSession
  case reply of
    Left err -> putStrLn $ methodErrorMessage err
    Right _  -> return ()

callGetSession :: IO (Maybe String)
callGetSession = do
  reply <- callMethod $ methodCall path interface memGetSession
  case reply of
    Left err   -> putStrLn (methodErrorMessage err) >> return Nothing
    Right body -> return $ getBodySession body

getBodySession :: [Variant] -> Maybe String
getBodySession [b] = case ses of { Just "" -> Nothing; _ -> ses }
  where
    ses = fromVariant b :: Maybe String
getBodySession _ = Nothing

callMethod :: MethodCall -> IO (Either MethodError [Variant])
callMethod mc = do
  client <- connectSession
  reply <- call client mc { methodCallDestination = Just busname }
  disconnect client
  return $ methodReturnBody <$> reply
