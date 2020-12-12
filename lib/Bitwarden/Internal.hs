{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Bitwarden.Internal
  ( Item(..)
  , Login(..)
  , Session
  , runDaemon
  , runClient
  , getItems
  , callGetSession
  ) where

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

import           System.Clipboard
import           System.Process

--------------------------------------------------------------------------------
-- | Daemon
--
-- Daemon will export an interface on DBus with two methods:
-- * get current session id -  if no session is active, launch Rofi to prompt
--   for a password; return session id or null if password is invalid
-- * lock session - destroy the current session id if active
--
-- The session ID will be valid only as long as TIMEOUT

newtype BWServerConf = BWServerConf
    { timeout :: UnixDiffTime
    }

-- TODO add a cache so the browse list will load faster
data CurrentSession = CurrentSession
    { timestamp :: UnixTime
    , hash      :: String
    }

type Session = MVar (Maybe CurrentSession)

runDaemon :: Int -> IO ()
runDaemon t = do
  ses <- newMVar Nothing
  let c = BWServerConf { timeout = UnixDiffTime (fromIntegral t) 0 }
  startService c ses
  forever $ threadDelay 1000000

lockSession :: Session -> IO ()
lockSession ses = void $ swapMVar ses Nothing

syncSession :: BWServerConf -> Session -> IO ()
syncSession conf ses = notify =<< fmap join . mapM cmd =<< getSession' conf ses
  where
    cmd h = readCmdSuccess "bw" ["sync", "--session", h] ""
    notify res = let j = isJust res
      in notifyStatus j $ if j then "sync succeeded" else "sync failed"

getSession' :: BWServerConf -> Session -> IO (Maybe String)
getSession' BWServerConf { timeout = t } ses = do
  ut <- getUnixTime
  modifyMVar ses $ \s -> case s of
    Just CurrentSession { timestamp = ts, hash = h } ->
      if diffUnixTime ut ts > t then getNewSession else return (s, Just h)
    Nothing -> getNewSession
  where
    getNewSession = do
      pwd <- readPassword
      newHash <- join <$> mapM readSession pwd
      (, newHash) <$> mapM newSession newHash
    newSession h = do
      ut <- getUnixTime
      return CurrentSession { timestamp = ut, hash = h }

getSession :: BWServerConf -> Session -> IO String
getSession conf ses = fromMaybe "" <$> getSession' conf ses

readSession :: String -> IO (Maybe String)
readSession pwd = readCmdSuccess "bw" ["unlock", pwd, "--raw"] ""

notifyStatus :: Bool -> String -> IO ()
notifyStatus succeeded msg =
  void $ spawnProcess "notify-send" ["-i", i, msg]
  where
    i = if succeeded
      then "dialog-information-symbolic"
      else "dialog-error-symbolic"

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

newtype BWClientConf = BWClientConf [String]

instance RofiConf BWClientConf where
  defArgs (BWClientConf a) = a

runClient :: [String] -> IO ()
runClient a = do
  let c = BWClientConf a
  runRofiIO c $ selectAction $ emptyMenu
    { groups = [untitledGroup $ toRofiActions ras]
    , prompt = Just "Action"
    }
  where
    ras = [ ("Browse Logins", browseLogins)
          , ("Sync Session", io callSyncSession)
          , ("Lock Session", io callLockSession)
          ]

browseLogins :: RofiConf c => RofiIO c ()
browseLogins = do
  session <- io callGetSession
  forM_ session $ (io . getItems) >=> selectItem

-- TODO use this in rofi-dev to mount thing using BW passwords
getItems :: String -> IO [Item]
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
selectItem :: RofiConf c => [Item] -> RofiIO c ()
selectItem items = selectAction $ emptyMenu
  { groups = [untitledGroup $ itemsToRofiActions items]
  , prompt = Just "Login"
  }

itemsToRofiActions :: RofiConf c => [Item] -> RofiActions c
itemsToRofiActions = toRofiActions . fmap (\i -> (name i, selectCopy $ login i))

selectCopy :: RofiConf c => Login -> RofiIO c ()
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

loginToRofiActions :: RofiConf c => Login -> (String -> RofiIO c ()) -> RofiActions c
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

startService :: BWServerConf -> Session -> IO ()
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
      , autoMethod memSyncSession $ syncSession c ses
      ]
    }

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

memSyncSession :: MemberName
memSyncSession = "SyncSession"

callMember :: MemberName -> IO [Variant]
callMember m = do
  reply <- callMethod $ methodCall path interface m
  case reply of
    Left err   -> putStrLn (methodErrorMessage err) >> return []
    Right body -> return body

callLockSession :: IO ()
callLockSession = void $ callMember memLockSession

callSyncSession :: IO ()
callSyncSession = void $ callMember memSyncSession

callGetSession :: IO (Maybe String)
callGetSession = getBodySession <$> callMember memGetSession

getBodySession :: [Variant] -> Maybe String
getBodySession [b] = case fromVariant b :: Maybe String of
  Just "" -> Nothing
  s       -> s
getBodySession _ = Nothing

callMethod :: MethodCall -> IO (Either MethodError [Variant])
callMethod mc = do
  client <- connectSession
  reply <- call client mc { methodCallDestination = Just busname }
  disconnect client
  return $ methodReturnBody <$> reply
