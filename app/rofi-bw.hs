{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | rofi-bw - a rofi prompt for a bitwarden vault
--
-- This is basically a wrapper around the 'bw' command, which is assumed to be
-- properly configured before running this command. This shows a system of
-- menus that allows easy lookup of data associated with a vault entry. For now
-- only lookups (no edits or creation) are supported, and only logins can be
-- searched. Any searched entry can be copied to the clipboard
--
-- In order to manage the session keys, this utility is split into a daemon and
-- client (the former holds the session keys between calls with the latter).
-- They communicate via dbus.
--
-- Most of the heavy-lifting code for this executable is in Bitwarden.Internal
-- to allow parts of this greater rofi library to use the DBus API

module Main (main) where

import           Bitwarden.Internal

import           Control.Monad

import           Data.Maybe

import           Rofi.Command

import           Text.Read

import           System.Directory
import           System.Environment
import           System.Exit

main :: IO ()
main = runChecks >> getArgs >>= parse

-- TODO check if daemon is running when running client
parse :: [String] -> IO ()
parse ["-d", t] = case readMaybe t of { Just t' -> runDaemon t'; _ -> usage }
parse ("-c":args) = runClient args
parse _         = usage

usage :: IO ()
usage = putStrLn $ joinNewline
  [ "daemon mode: rofi-bw -d TIMEOUT"
  , "client mode: rofi-bw -c [ROFI-ARGS]"
  ]

runChecks :: IO ()
runChecks = checkExe "bw" >> checkExe "rofi"

checkExe :: String -> IO ()
checkExe cmd = do
  res <- findExecutable cmd
  unless (isJust res) $ do
    putStrLn $ "Could not find executable: " ++ cmd
    exitWith $ ExitFailure 1
