--------------------------------------------------------------------------------
-- | rofi-pinentry - a simply pinentry proxy for bitwarden
--
-- Rather than prompt the user like all the other pinentry programs, call the
-- bitwarden deamon and prompt for a password there

module Main where

import           Data.List

import           Bitwarden.Internal
import           System.Environment
import           System.Exit

main :: IO ()
main = do
  n <- parseArgs =<< getArgs
  putStrLn "Hello loser"
  pinentryLoop n

parseArgs :: [String] -> IO String
parseArgs [n] = return n
parseArgs _ = do
  putStrLn "Usage: pinentry-rofi [BWNAME]"
  exitWith $ ExitFailure 1

pinentryLoop :: String -> IO ()
pinentryLoop n = do
  c <- getLine
  processLine n $ words c
  pinentryLoop n

processLine :: String -> [String] -> IO ()
processLine _ []                             = noop
processLine _ ["BYE"]                        = exitSuccess
processLine n ["GETPIN"]                     = getPin n

-- TODO this might be important
processLine _ ["OPTION", o]                  = processOption o

-- TODO this might be important
processLine _ ["GETINFO", _]                 = noop

-- these all take one arg and should do nothing here
processLine _ ["SETDESC", _]                 = noop
processLine _ ["SETOK", _]                   = noop
processLine _ ["SETNOTOK", _]                = noop
processLine _ ["SETCANCEL", _]               = noop
processLine _ ["SETPROMPT", _]               = noop
processLine _ ["SETERROR", _]                = noop

-- CONFIRM can take a flag
processLine _ ["CONFIRM"]                    = noop
processLine _ ["CONFIRM", "--one-button", _] = noop

processLine _ ss                             = unknownCommand $ unwords ss

unknownCommand :: String -> IO ()
unknownCommand c = putStrLn $ "ERR 275 Unknown command " ++ c

getPin :: String -> IO ()
getPin n = do
  its <- getItems
  let p = (password . login) =<< find (\i -> n == name i) its
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
