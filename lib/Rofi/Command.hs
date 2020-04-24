{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rofi.Command
  ( RofiConf(..)
  , RofiMenu(..)
  , RofiAction
  , RofiActions
  , RofiPrompt
  , Hotkey(..)
  , io
  , emptyMenu
  , runRofiPrompt
  , toRofiActions
  , rofiActionKeys
  , untitledGroup
  , titledGroup
  , selectAction
  , readCmdSuccess
  , readCmdEither
  , dmenuArgs
  , joinNewline
  , stripWS
  ) where

import           Control.Monad.Reader

import           Data.Char
import           Data.List
import qualified Data.Map.Ordered     as M
import           Data.Maybe

import           System.Exit
import           System.Process

newtype RofiConf = RofiConf
    { defArgs :: [String]
    }
  deriving (Eq, Show)

type RofiAction = (String, RofiPrompt ())

type RofiActions = M.OMap String (RofiPrompt ())

data RofiGroup = RofiGroup
    { actions :: RofiActions
    , title   :: Maybe String
    }

untitledGroup :: RofiActions -> RofiGroup
untitledGroup a = RofiGroup { actions = a, title = Nothing }

titledGroup :: String -> RofiActions -> RofiGroup
titledGroup t a = (untitledGroup a) { title = Just t }

data Hotkey = Hotkey
    { keyCombo       :: String
    -- only 1-10 are valid
    , keyIndex       :: Int
    , keyDescription :: String
    , keyActions     :: RofiActions
    }

hotkeyBinding :: Hotkey -> [String]
hotkeyBinding Hotkey { keyIndex = e, keyCombo = c } = [k, c]
  where
    k = "-kb-custom-" ++ show e

hotkeyMsg1 :: Hotkey -> String
hotkeyMsg1 Hotkey { keyCombo = c, keyDescription = d } =
  c ++ ": <i>" ++ d ++ "</i>"

hotkeyMsg :: [Hotkey] -> [String]
hotkeyMsg [] = []
hotkeyMsg hs = ["-mesg", intercalate " | " $ fmap hotkeyMsg1 hs]

hotkeyArgs :: [Hotkey] -> [String]
hotkeyArgs hks = hotkeyMsg hks ++ concatMap hotkeyBinding hks

data RofiMenu = RofiMenu
    { groups  :: [RofiGroup]
    , prompt  :: Maybe String
    , hotkeys :: [Hotkey]
    }

emptyMenu :: RofiMenu
emptyMenu = RofiMenu
  { groups = []
  , prompt = Nothing
  , hotkeys = []
  }

newtype RofiPrompt a = RofiPrompt (ReaderT RofiConf IO a)
    deriving (Functor, Monad, MonadIO, MonadReader RofiConf)

instance Applicative RofiPrompt where
  pure = return
  (<*>) = ap

io :: IO a -> RofiPrompt a
io = liftIO

runRofiPrompt :: RofiConf -> RofiPrompt a -> IO a
runRofiPrompt c (RofiPrompt a) = runReaderT a c

toRofiActions :: [(String, RofiPrompt ())] -> RofiActions
toRofiActions = M.fromList

rofiActionKeys :: RofiActions -> String
rofiActionKeys = joinNewline . map fst . M.assocs

lookupRofiAction :: String -> RofiActions -> RofiPrompt ()
lookupRofiAction key ras = fromMaybe (return ()) $ M.lookup key ras

groupEntries :: RofiGroup -> String
groupEntries RofiGroup { actions = a, title = t }
  | null a = ""
  | otherwise = title' ++ rofiActionKeys a
  where
    title' = maybe "" (++ "\n") t

menuActions :: RofiMenu -> RofiActions
menuActions = foldr1 (M.<>|) . fmap actions . groups

menuEntries :: RofiMenu -> String
menuEntries = intercalate "\n\n" . fmap groupEntries . groups

selectAction :: RofiMenu -> RofiPrompt ()
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

readRofi :: [String] -> String -> RofiPrompt (Either (Int, String, String) String)
readRofi uargs input = do
  dargs <- asks defArgs
  io $ readCmdEither "rofi" (dmenuArgs ++ dargs ++ uargs) input

readCmdSuccess :: String -> [String] -> String -> IO (Maybe String)
readCmdSuccess cmd args input = either (const Nothing) Just
  <$> readCmdEither cmd args input

readCmdEither :: String -> [String] -> String -> IO (Either (Int, String, String) String)
readCmdEither cmd args input = do
  (ec, out, err) <- readProcessWithExitCode cmd args input
  return $ case ec of
    ExitSuccess   -> Right $ stripWS out
    ExitFailure n -> Left (n, stripWS out, stripWS err)

stripWS :: String -> String
stripWS = reverse . dropWhile isSpace . reverse

joinNewline :: [String] -> String
joinNewline = intercalate "\n"
