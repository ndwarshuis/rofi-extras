--------------------------------------------------------------------------------
-- | rofi-autorandr - a rofi prompt to select autorandr profiles
--
-- Simple wrapper to select an autorandr profile.


module Main (main) where

import           Control.Monad

import           Data.Maybe

import           Rofi.Command

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.Process

main :: IO ()
main = runChecks >> getArgs >>= runPrompt

-- TOOD not DRY
runChecks :: IO ()
runChecks = checkExe "autorandr" >> checkExe "rofi"

checkExe :: String -> IO ()
checkExe cmd = do
  res <- findExecutable cmd
  unless (isJust res) $ do
    putStrLn $ "Could not find executable: " ++ cmd
    exitWith $ ExitFailure 1

newtype ARClientConf = ARClientConf [String]

instance RofiConf ARClientConf where
  defArgs (ARClientConf a) = a

runPrompt :: [String] -> IO ()
runPrompt a = do
  let c = ARClientConf a
  profs <- getAutoRandrProfiles
  runRofiIO c $ selectAction $ emptyMenu
    { groups = [untitledGroup $ toRofiActions [(p, selectProfile p) | p <- profs]]
    , prompt = Just "Select Profile"
    }

-- TODO filter profiles based on which xrandr outputs are actually connected
getAutoRandrProfiles :: IO [String]
getAutoRandrProfiles = do
  dir <- getAutoRandrDir
  contents <- listDirectory dir
  filterM (doesDirectoryExist . (dir </>)) contents

getAutoRandrDir :: IO String
getAutoRandrDir = do
  x <- xdgDir
  res <- doesDirectoryExist x
  if res then return x else legacyDir
  where
    xdgDir = do
      e <- lookupEnv "XDG_CONFIG_HOME"
      case e of
        Nothing -> appendToHome "./config/autorandr"
        Just p  -> return $ p </> "autorandr"
    legacyDir = appendToHome ".autorandr"
    appendToHome p = (</> p) <$> getHomeDirectory

selectProfile :: String -> RofiIO ARClientConf ()
selectProfile name = do
  io $ putStrLn name
  io $ void $ spawnProcess "autorandr" ["--change", name]
