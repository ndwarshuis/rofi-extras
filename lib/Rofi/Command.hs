{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rofi.Command
  ( RofiConf(..)
  , RofiMenu(..)
  , RofiAction
  , RofiActions
  , RofiIO
  , RofiGroup
  , Hotkey(..)
  , io
  , emptyMenu
  , runRofiIO
  , toRofiActions
  , rofiActionKeys
  , untitledGroup
  , titledGroup
  , selectAction
  , readPassword
  , readCmdSuccess
  , readCmdEither
  , readCmdEither'
  , dmenuArgs
  , joinNewline
  , stripWS
  ) where

import           Control.Monad.IO.Unlift
import           Control.Monad.Reader

import           Data.Char
import           Data.List
import qualified Data.Map.Ordered        as M
import           Data.Maybe

import           System.Exit
import           System.Process

class RofiConf c where
  defArgs :: c -> [String]

type RofiAction c = (String, RofiIO c ())

type RofiActions c = M.OMap String (RofiIO c ())

data RofiGroup c = RofiGroup
    { actions :: RofiActions c
    , title   :: Maybe String
    }

untitledGroup :: RofiActions c -> RofiGroup c
untitledGroup a = RofiGroup { actions = a, title = Nothing }

titledGroup :: String -> RofiActions c -> RofiGroup c
titledGroup t a = (untitledGroup a) { title = Just t }

data Hotkey c = Hotkey
    { keyCombo       :: String
    -- only 1-10 are valid
    , keyIndex       :: Int
    , keyDescription :: String
    , keyActions     :: RofiActions c
    }

hotkeyBinding :: Hotkey c -> [String]
hotkeyBinding Hotkey { keyIndex = e, keyCombo = c } = [k, c]
  where
    k = "-kb-custom-" ++ show e

hotkeyMsg1 :: Hotkey c -> String
hotkeyMsg1 Hotkey { keyCombo = c, keyDescription = d } =
  c ++ ": <i>" ++ d ++ "</i>"

hotkeyMsg :: [Hotkey c] -> [String]
hotkeyMsg [] = []
hotkeyMsg hs = ["-mesg", intercalate " | " $ fmap hotkeyMsg1 hs]

hotkeyArgs :: [Hotkey c] -> [String]
hotkeyArgs hks = hotkeyMsg hks ++ concatMap hotkeyBinding hks

data RofiMenu c = RofiMenu
    { groups  :: [RofiGroup c]
    , prompt  :: Maybe String
    , hotkeys :: [Hotkey c]
    }

emptyMenu :: RofiMenu c
emptyMenu = RofiMenu
  { groups = []
  , prompt = Nothing
  , hotkeys = []
  }

newtype RofiIO c a = RofiIO (ReaderT c IO a)
    deriving (Functor, Monad, MonadIO, MonadReader c, MonadUnliftIO)

instance Applicative (RofiIO c) where
  pure = return
  (<*>) = ap

io :: MonadIO m => IO a -> m a
io = liftIO

runRofiIO :: c -> RofiIO c a -> IO a
runRofiIO c (RofiIO r) = runReaderT r c

toRofiActions :: [(String, RofiIO c ())] -> RofiActions c
toRofiActions = M.fromList

rofiActionKeys :: RofiActions c -> String
rofiActionKeys = joinNewline . map fst . M.assocs

lookupRofiAction :: String -> RofiActions c -> RofiIO c ()
lookupRofiAction key ras = fromMaybe (return ()) $ M.lookup key ras

groupEntries :: RofiGroup c -> String
groupEntries RofiGroup { actions = a, title = t }
  | null a = ""
  | otherwise = title' ++ rofiActionKeys a
  where
    title' = maybe "" (++ "\n") t

menuActions :: RofiMenu c -> RofiActions c
menuActions = foldr1 (M.<>|) . fmap actions . groups

menuEntries :: RofiMenu c -> String
menuEntries = intercalate "\n\n" . filter (not . null) . fmap groupEntries . groups

selectAction :: RofiConf c => RofiMenu c -> RofiIO c ()
selectAction rm = do
  let p = maybeOption "-p" $ prompt rm
  let hArgs = hotkeyArgs $ hotkeys rm
  res <- readRofi (p ++ hArgs) $ menuEntries rm
  case res of
    Right key        -> lookupRofiAction key $ menuActions rm
    Left (n, key, _) -> mapM_ (lookupRofiAction key . keyActions)
      $ find ((==) n . (+ 9) . keyIndex)
      $ hotkeys rm

maybeOption :: String -> Maybe String -> [String]
maybeOption switch = maybe [] (\o -> [switch, o])

dmenuArgs :: [String]
dmenuArgs = ["-dmenu"]

readRofi :: RofiConf c => [String]
  -> String
  -> RofiIO c (Either (Int, String, String) String)
readRofi uargs input = do
  dargs <- asks defArgs
  io $ readCmdEither "rofi" (dmenuArgs ++ dargs ++ uargs) input

readCmdSuccess :: String -> [String] -> String -> IO (Maybe String)
readCmdSuccess cmd args input = either (const Nothing) Just
  <$> readCmdEither cmd args input

readCmdEither :: String
  -> [String]
  -> String
  -> IO (Either (Int, String, String) String)
readCmdEither cmd args input = resultToEither
  <$> readProcessWithExitCode cmd args input

readCmdEither' :: String
  -> [String]
  -> String
  -> [(String, String)]
  -> IO (Either (Int, String, String) String)
readCmdEither' cmd args input environ = resultToEither
  <$> readCreateProcessWithExitCode p input
  where
    p = (proc cmd args) { env = Just environ }

resultToEither :: (ExitCode, String, String)
  -> Either (Int, String, String) String
resultToEither (ExitSuccess, out, _)     = Right $ stripWS out
resultToEither (ExitFailure n, out, err) = Left (n, stripWS out, stripWS err)

stripWS :: String -> String
stripWS = reverse . dropWhile isSpace . reverse

joinNewline :: [String] -> String
joinNewline = intercalate "\n"

readPassword :: IO (Maybe String)
readPassword = readCmdSuccess "rofi" args ""
  where
    args = dmenuArgs ++ ["-p", "Password", "-password"]
