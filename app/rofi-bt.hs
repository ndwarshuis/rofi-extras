--------------------------------------------------------------------------------
-- | rofi-bt - a prompt to dicsonnect/connect devices
--

module Main (main) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader

import           Data.List
import           Data.List.Split
import qualified Data.Map             as M
import           Data.Maybe

import           DBus
import           DBus.Client

import           Rofi.Command

main :: IO ()
main = runPrompt

-- dummy type with nothing in it since there is nothing to configure for this
-- (yet)
newtype RofiBTConf = RofiBTConf ObjectPath

instance RofiConf RofiBTConf where
  defArgs (RofiBTConf _) = []

type BTAction = RofiAction RofiBTConf

runPrompt :: IO ()
runPrompt = do
  c <- getClient
  maybe (putStrLn "could not get DBus client") run c
  where
    run client = do
      paths <- M.keys <$> getObjectTree client
      maybe (putStrLn "could not get DBus adapter") (actions client paths)
        $ getAdapter paths
    actions client paths adapter = do
      ras <- getRofiActions client paths
      runRofiIO (RofiBTConf adapter) $ selectAction $ emptyMenu
        { groups = [untitledGroup $ toRofiActions ras]
        , prompt = Just "Select Device"
        }

getRofiActions :: Client -> [ObjectPath] -> IO [BTAction]
getRofiActions client os = do
  devs <- getDevices client os
  catMaybes <$> mapM (deviceToRofiAction client) devs

deviceToRofiAction :: Client -> ObjectPath -> IO (Maybe BTAction)
deviceToRofiAction client dev = do
  c <- getDeviceConnected client dev
  n <- getDeviceName client dev
  return $ case (c, n) of
    (Just c', Just n') -> Just ( formatDeviceEntry c' n'
                               , powerAdapterMaybe client >> io (mkAction c')
                               )
    _                  -> Nothing
  where
    mkAction True  = callDeviceDisconnect client dev
    mkAction False = callDeviceConnect client dev

powerAdapterMaybe :: Client -> RofiIO RofiBTConf ()
powerAdapterMaybe client = do
  (RofiBTConf adapter) <- ask
  let mc = btMethodCall adapter i m
  let powerOnMaybe = flip unless $ void $ setProperty client mc value
  powered <- io $ getBTProperty client adapter i m
  io $ maybe (putStrLn "could not get adapter powered status") powerOnMaybe powered
  where
    i = interfaceName_ "org.bluez.Adapter1"
    m = memberName_ "Powered"
    -- apparently this needs to be double-variant'd to match the signature of
    -- the 'Set' method
    value = toVariant $ toVariant True

formatDeviceEntry :: Bool -> String -> String
formatDeviceEntry connected name = unwords [prefix connected, name]
  where
    prefix True  = "#"
    prefix False = " "

getAdapter :: [ObjectPath] -> Maybe ObjectPath
getAdapter = find pathIsAdaptor

getDevices :: Client -> [ObjectPath] -> IO [ObjectPath]
getDevices client = filterM (getDevicePaired client) . filter pathIsDevice

type ObjectTree = M.Map ObjectPath (M.Map String (M.Map String Variant))

getObjectTree :: Client -> IO ObjectTree
getObjectTree client =
  fromMaybe M.empty . eitherMaybe from <$> callBTMethod client o i m
  where
    o = objectPath_ "/"
    i = interfaceName_ "org.freedesktop.DBus.ObjectManager"
    m = memberName_ "GetManagedObjects"
    from = fromVariant <=< listToMaybe . methodReturnBody

getDeviceConnected :: Client -> ObjectPath -> IO (Maybe Bool)
getDeviceConnected = getDevProperty "Connected"

getDeviceName :: Client -> ObjectPath -> IO (Maybe String)
getDeviceName = getDevProperty "Name"

getDevicePaired :: Client -> ObjectPath -> IO Bool
getDevicePaired c = fmap (fromMaybe False) . getDevProperty "Paired" c

callDeviceConnect :: Client -> ObjectPath -> IO ()
callDeviceConnect = callDevMethod "Connect"

callDeviceDisconnect :: Client -> ObjectPath -> IO ()
callDeviceDisconnect = callDevMethod "Disconnect"

pathIsAdaptor :: ObjectPath -> Bool
pathIsAdaptor o = case splitPath o of
  [a, b, c] -> pathIsAdaptorPrefix a b c
  _         -> False

pathIsDevice :: ObjectPath -> Bool
pathIsDevice o = case splitPath o of
  [a, b, c, _] -> pathIsAdaptorPrefix a b c
  _            -> False

pathIsAdaptorPrefix :: String -> String -> String -> Bool
pathIsAdaptorPrefix a b c = a == "org" && b == "bluez" && "hci" `isPrefixOf` c

splitPath :: ObjectPath -> [String]
splitPath =splitOn "/" . dropWhile (=='/') . formatObjectPath

getClient :: IO (Maybe Client)
getClient = either warn (return . Just) =<< try connectSystem
  where
    warn e = putStrLn (clientErrorMessage e) >> return Nothing

callDevMethod :: String -> Client -> ObjectPath -> IO ()
callDevMethod mem client dev =
  void $ callBTMethod client dev btDevInterface $ memberName_ mem

getDevProperty :: IsVariant a => String -> Client -> ObjectPath -> IO (Maybe a)
getDevProperty mem client dev =
  getBTProperty client dev btDevInterface $ memberName_ mem

callBTMethod :: Client -> ObjectPath -> InterfaceName
  -> MemberName -> IO (Either MethodError MethodReturn)
callBTMethod client o i m = call client (btMethodCall o i m)
  -- eitherMaybe (fromVariant <=< listToMaybe . methodReturnBody)
  -- <$> call client (btMethodCall o i m)

getBTProperty :: IsVariant a => Client -> ObjectPath
  -> InterfaceName -> MemberName -> IO (Maybe a)
getBTProperty client o i m =
  eitherMaybe fromVariant <$> getProperty client (btMethodCall o i m)

btMethodCall :: ObjectPath -> InterfaceName -> MemberName -> MethodCall
btMethodCall o i m = (methodCall o i m) { methodCallDestination = Just btBus }

eitherMaybe :: (b -> Maybe c) -> Either a b -> Maybe c
eitherMaybe = either (const Nothing)

btBus :: BusName
btBus = busName_ "org.bluez"

btDevInterface :: InterfaceName
btDevInterface = interfaceName_ "org.bluez.Device1"
