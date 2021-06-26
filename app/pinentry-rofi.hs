--------------------------------------------------------------------------------
-- | rofi-pinentry - a simply pinentry proxy for bitwarden
--
-- Rather than prompt the user like all the other pinentry programs, call the
-- bitwarden deamon and prompt for a password there

module Main where

import           Data.List

import           Bitwarden.Internal
import           System.Exit

main :: IO ()
main = do
  putStrLn "Hello loser"
  pinentryLoop

pinentryLoop :: IO ()
pinentryLoop = do
  c <- getLine
  processLine $ words c
  pinentryLoop

processLine :: [String] -> IO ()
processLine []                             = noop
processLine ["BYE"]                        = exitSuccess
processLine ["GETPIN"]                     = getPin

-- TODO this might be important
processLine ["OPTION", o]                  = processOption o

-- TODO this might be important
processLine ["GETINFO", _]                 = noop

-- these all take one arg and should do nothing here
processLine ["SETDESC", _]                 = noop
processLine ["SETOK", _]                   = noop
processLine ["SETNOTOK", _]                = noop
processLine ["SETCANCEL", _]               = noop
processLine ["SETPROMPT", _]               = noop
processLine ["SETERROR", _]                = noop

-- CONFIRM can take a flag
processLine ["CONFIRM"]                    = noop
processLine ["CONFIRM", "--one-button", _] = noop

processLine ss                             = unknownCommand $ unwords ss

unknownCommand :: String -> IO ()
unknownCommand c = putStrLn $ "ERR 275 Unknown command " ++ c

-- TODO make this a CLI arg
gpgname :: String
gpgname = "password name"

getPin :: IO ()
getPin = do
  its <- getItems
  let p = (password . login) =<< find (\i -> gpgname == name i) its
  maybe err printPin p
  where
    err = putStrLn "ERR 83886179 Operation canceled <rofi>"
    printPin p = putStrLn ("D " ++ p) >> ok

processOption :: String -> IO ()
processOption = undefined

noop :: IO ()
noop = ok

ok :: IO ()
ok = putStrLn "OK"
