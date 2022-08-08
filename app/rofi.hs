module Main (main) where

--------------------------------------------------------------------------------
-- | Run rofi (and display on the correct screen)
--
-- Since this seems random, the reason for this is that I want rofi to appear
-- over the current xmonad workspace, and rofi has no concept of what an
-- xmonad workspace is (not that it is supposed to, xmonad is weird...). Rofi
-- accepts the name of an xrandr output onto which it should appear, so this
-- binary determines which xmonad workspace is in focus and calls rofi with the
-- name of that workspace.
--
-- Assumptions: xmonad sets the _NET_DESKTOP_VIEWPORT atom with the positions of
-- the active workspace (actually an array of the positions of all workspaces
-- but the rest don't matter if I only care about the active one). This is not
-- default behavior and not in any contrib modules (yet) so I added this myself
-- using a custom loghook.
--
-- Steps:
-- 1) Get _NET_CURRENT_DESKTOP to find index of active workspace
-- 2) Use index from (1) and to get the position of the active workspace from
--    _NET_DESKTOP_VIEWPORT
-- 3) Find the name of the xrandr output whose position matches that from (2)
-- 4) Call rofi with the '-m' flag to override the default monitor placement

import           Data.Maybe

import           Graphics.X11.Types
import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xrandr

import           System.Environment
import           System.Process

main :: IO ()
main = do
  r <- getMonitorName
  let pre = maybe [] (\n -> ["-m", n]) r
  args <- getArgs
  callProcess "/usr/bin/rofi" $ pre ++ args

data Coord = Coord Int Int deriving (Eq, Show)

getMonitorName :: IO (Maybe String)
getMonitorName = do
  dpy <- openDisplay ""
  root <- rootWindow dpy $ defaultScreen dpy
  index <- getCurrentDesktopIndex dpy root
  viewports <- getDesktopViewports dpy root
  outputs <- getOutputs dpy root
  return $ flip lookup outputs =<< (viewports !!?) =<< index

getCurrentDesktopIndex :: Display -> Window -> IO (Maybe Int)
getCurrentDesktopIndex dpy root =
  (!!? 0) <$> getAtom32 dpy root "_NET_CURRENT_DESKTOP"

getDesktopViewports :: Display -> Window -> IO [Coord]
getDesktopViewports dpy root =
  pairs <$> getAtom32 dpy root "_NET_DESKTOP_VIEWPORT"
  where
    pairs = reverse . pairs' []
    pairs' acc (x1:x2:xs) = pairs' (Coord x1 x2 : acc) xs
    pairs' acc _          = acc

getOutputs :: Display -> Window -> IO [(Coord, String)]
getOutputs dpy root = xrrGetScreenResourcesCurrent dpy root >>=
  maybe (return []) resourcesToCells
  where
    resourcesToCells r = catMaybes <$> mapM (outputToCell r) (xrr_sr_outputs r)
    outputToCell r o = xrrGetOutputInfo dpy r o >>= infoToCell r
    -- connection: 0 == connected, 1 == disconnected
    infoToCell r (Just XRROutputInfo { xrr_oi_connection = 0
                                     , xrr_oi_name = n
                                     , xrr_oi_crtc = c
                                     }) = do
      fmap (\i -> (toCoord i, n)) <$> xrrGetCrtcInfo dpy r c
    infoToCell _ _ = return Nothing
    toCoord c = Coord (fromIntegral $ xrr_ci_x c) (fromIntegral $ xrr_ci_y c)

infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = listToMaybe $ drop i xs

getAtom32 :: Display -> Window -> String -> IO [Int]
getAtom32 dpy root str = do
  a <- internAtom dpy str False
  p <- getWindowProperty32 dpy a root
  return $ maybe [] (fmap fromIntegral) p
