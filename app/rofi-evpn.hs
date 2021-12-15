--------------------------------------------------------------------------------
-- | rofi-evpn - a prompt to dicsonnect/connect with express VPN
--

module Main (main) where

import           Control.Monad

import           Data.List.Split
import           Data.Maybe

import           Rofi.Command

import           System.Environment
import           System.Process

main :: IO ()
main = getArgs >>= runPrompt

runPrompt :: [String] -> IO ()
runPrompt args = do
  servers <- getServers
  maybe (return ()) run servers
  where
    run (VPNStatus connected servers) = do
      let d = getDisconnectAction <$> connected
      let cs = fmap (getConnectAction connected) servers
      runRofiIO (RofiVPNConf args) $ selectAction $ emptyMenu
        { groups =
          [ untitledGroup $ toRofiActions $ maybeToList d
          , untitledGroup $ toRofiActions cs
          ]
        , prompt = Just "Select Action"
        }

newtype RofiVPNConf = RofiVPNConf [String]

instance RofiConf RofiVPNConf where
  defArgs (RofiVPNConf as) = as

type VPNAction = RofiAction RofiVPNConf

type VPNServer = (String, String)

data VPNStatus = VPNStatus (Maybe String) [VPNServer] deriving (Show)

getServers :: IO (Maybe VPNStatus)
getServers = do
  running <- daemonIsRunning
  if running
    then Just <$> getStatus
    else notify IconError "ExpressVPN daemon not running" >> return Nothing

getStatus :: IO VPNStatus
getStatus = do
  connected <- getConnectedServer
  VPNStatus connected <$> getAvailableServers

getConnectedServer :: IO (Maybe String)
getConnectedServer = (procStatus =<<) <$> readCmdSuccess eVPN ["status"] ""
  where
    procStatus s = case words <$> lines s of
      -- the output is green...
      (("\ESC[1;32;49mConnected":"to":server):_) -> Just $ unwords server
      _                                          -> Nothing

getAvailableServers :: IO [VPNServer]
getAvailableServers = procOut =<< readCmdSuccess eVPN ["ls"] ""
  where
    procOut Nothing   = do
      notify IconError "failed to get list of servers"
      return []
    -- ASSUME the output has a header that has three lines, followed by the
    -- stuff we care about, which is followed by a blank line (after which there
    -- is other stuff that doesn't matter based on the way I'm parsing below)
    procOut (Just ls) = return
      $ mapMaybe (matchLine . splitOn "\t")
      $ drop 3
      $ takeWhile (/= "")
      $ lines ls
    -- The output of this command is very strange; it is delimited (kinda) by
    -- tabs but some lines are long enough that they don't have a tab. In
    -- whatever case, splitting by tabs leads to variable length lists, and the
    -- id is always at the front and the location is always at the end. These
    -- should handle all cases.
    matchLine [i, _, l]       = Just (i, l)
    matchLine [i, _, _, l]    = Just (i, l)
    matchLine [i, _, _, _, l] = Just (i, l)
    matchLine _               = Nothing

daemonIsRunning :: IO Bool
daemonIsRunning = isJust <$> readCmdSuccess "pgrep" [eVPND] ""

getDisconnectAction :: String -> VPNAction
getDisconnectAction server =
  ("Disconnect from " ++ server, io $ void $ disconnect server)

getConnectAction :: Maybe String -> VPNServer -> VPNAction
getConnectAction connected server =
  (formatServerLine server, io $ go connected)
  where
    go (Just c) = do
      success <- disconnect c
      when success con
    go _ = con
    con = connect server

formatServerLine :: VPNServer -> String
formatServerLine (sid, sname) = pad sid ++ " | " ++ sname
  where
    pad s = s ++ replicate (10 - length s) ' '

eVPN :: String
eVPN = "expressvpn"

eVPND :: String
eVPND = "expressvpnd"

connect :: VPNServer -> IO ()
connect (sid, sname) = do
  res <- readCmdSuccess' eVPN ["connect", sid]
  notifyIf res ("connected to " ++ sname)
    ("failed to connect to " ++ sname)

disconnect :: String -> IO Bool
disconnect server = do
  res <- readCmdSuccess' eVPN ["disconnect"]
  notifyIf res ("disconnected from " ++ server)
    ("failed to disconnect from " ++ server)
  return res

readCmdSuccess' :: String -> [String] -> IO Bool
readCmdSuccess' cmd args = isJust <$> readCmdSuccess cmd args ""

-- TODO not DRY
data NotifyIcon = IconError | IconInfo

instance Show NotifyIcon where
  show IconError = "dialog-error-symbolic"
  show IconInfo  = "dialog-information-symbolic"

notifyIf :: Bool -> String -> String -> IO ()
notifyIf True s _  = notify IconInfo s
notifyIf False _ s = notify IconError s

notify :: NotifyIcon -> String -> IO ()
notify icon body = void $ spawnProcess "notify-send" $ args ++ [body]
  where
    args = ["-i", show icon, summary]
    summary = "ExpressVPN"
