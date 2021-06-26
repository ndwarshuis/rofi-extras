{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | rofi-pinentry - a simply pinentry proxy for bitwarden
--
-- Rather than prompt the user like all the other pinentry programs, call the
-- bitwarden deamon and prompt for a password there

module Main where

import           Bitwarden.Internal

import           Data.List
import           Data.Yaml

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Posix.Process

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "OK Pleased to meet you"
  pinentryLoop =<< readPinConf

newtype PinConf = PinConf { pcBwName :: String } deriving (Eq, Show)

instance FromJSON PinConf where
  parseJSON (Object o) = PinConf <$> o .:? "bitwarden-name" .!= "gnupg"
  parseJSON _          = fail "pinentry yaml parse error"

readPinConf :: IO PinConf
readPinConf = do
  c <- decodeFileEither =<< pinConfDir
  case c of
    Left e  -> print e >> exitWith (ExitFailure 1)
    Right r -> return r

pinConfDir :: IO FilePath
pinConfDir = maybe defHome (return . (</> confname)) =<< lookupEnv "GNUPGHOME"
  where
    defHome = (</> ".gnupg" </> confname) <$> getHomeDirectory
    confname = "pinentry-rofi.yml"

pinentryLoop :: PinConf -> IO ()
pinentryLoop p = do
  processLine p . words =<< getLine
  pinentryLoop p

processLine :: PinConf -> [String] -> IO ()
processLine _ []                             = noop
processLine _ ["BYE"]                        = exitSuccess
processLine p ["GETPIN"]                     = getPin p

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

getPin :: PinConf -> IO ()
getPin p = do
  its <- getItems
  let w = (password . login) =<< find (\i -> pcBwName p == name i) its
  maybe err send w
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
