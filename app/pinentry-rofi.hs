--------------------------------------------------------------------------------
-- | rofi-pinentry - a simply pinentry proxy for bitwarden
--
-- Rather than prompt the user like all the other pinentry programs, call the
-- bitwarden deamon and prompt for a password there

module Main where

import           Data.List

import           Bitwarden.Internal
import           System.Exit
import           System.IO
import           System.Posix.Process

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  -- TODO don't hardcode this
  let n = "gnupg"
  putStrLn "OK Pleased to meet you"
  pinentryLoop n

pinentryLoop :: String -> IO ()
pinentryLoop n = do
  c <- getLine
  processLine n $ words c
  pinentryLoop n

processLine :: String -> [String] -> IO ()
processLine _ []                             = noop
processLine _ ["BYE"]                        = exitSuccess
processLine n ["GETPIN"]                     = getPin n

processLine _ ["GETINFO", o]                 = processGetInfo o

-- TODO this might be important
processLine _ ["OPTION", o]                  = processOption o

-- these should all do nothing
processLine _ ("SETDESC":_)                  = noop
processLine _ ("SETOK":_)                    = noop
processLine _ ("SETNOTOK":_)                 = noop
processLine _ ("SETCANCEL":_)                = noop
processLine _ ("SETPROMPT":_)                = noop
processLine _ ("SETERROR":_)                 = noop

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
  maybe err send p
  where
    err = putStrLn "ERR 83886179 Operation canceled <rofi>"

-- these are the only supported options for GETINFO; anything else is an error
processGetInfo :: String -> IO ()
processGetInfo "pid"     = send . show =<< getProcessID
processGetInfo "version" = noop
processGetInfo "flavor"  = noop
processGetInfo "ttyinfo" = noop
processGetInfo _         = putStrLn "ERR 83886360 IPC parameter error <rofi>"

processOption :: String -> IO ()
processOption _ = noop

send :: String -> IO ()
send s = putStrLn ("D " ++ s) >> ok

noop :: IO ()
noop = ok

ok :: IO ()
ok = putStrLn "OK"
