#!/usr/bin/env runhaskell

{-# LANGUAGE TupleSections #-}

import Control.Arrow (second)
import Control.Monad (join)

import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Foldable (fold, for_, toList)
import Data.Functor (void)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)

import qualified Data.List.NonEmpty as N

import System.Directory (XdgDirectory (..), canonicalizePath, doesDirectoryExist, getXdgDirectory, listDirectory)
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>))
import System.Process (readProcess, readProcessWithExitCode, spawnProcess)

-- random-shuffle
import System.Random.Shuffle (shuffleM)

main :: IO ()
main = join $ go <$> getArgs <*> lookupEnv "WPA_ID"
 where
  -- `wpa_cli -a`
  go [_, "CONNECTED"] (Just nid) = changeWallpaper =<< ssidByNid nid
  go [_, "DISCONNECTED"] Nothing = changeWallpaper Nothing
  -- manual refresh
  go [] Nothing = changeWallpaper . lookup "ssid" =<< getStatus
  -- manual override
  go [ssid] Nothing = changeWallpaper (Just ssid)
  go _ _ = fail "Invalid call"

  ssidByNid nid = do
    [_, raw] <- lines <$> readProcess "wpa_cli" ["get_network", nid, "ssid"] ""
    Just <$> readIO raw

  getStatus = do
    (_ : status) <- lines <$> readProcess "wpa_cli" ["status"] ""
    pure $ second (drop 1) . break (== '=') <$> status

changeWallpaper :: Maybe String -> IO ()
changeWallpaper ssid = do
  monitors <- getMonitors
  base <- getXdgDirectory XdgConfig "swap-background"
  pairing <- for monitors $ \monitor -> (monitor,) <$> getLocation base monitor

  for_ (N.groupAllWith snd pairing) $ \pairs -> do
    let monitors' = toList $ fst <$> pairs
    case snd $ N.head pairs of
      Nothing -> restartSwayBg $ applyFallback =<< monitors'
      Just source -> do
        wallpapers <- getShuffled monitors' =<< getWallpapers source
        restartSwayBg $ fold $ zipWith applyWallpaper monitors' wallpapers
 where
  network = fromMaybe "offline" ssid

  restartSwayBg args = do
    Just user <- lookupEnv "USER"
    void $ readProcessWithExitCode "pkill" ["-fu", user, "swaybg"] ""
    void $ spawnProcess "swaybg" args
  applyFallback monitor = ["-o", monitor, "-c", "0F0F0F"]
  applyWallpaper monitor wallpaper = ["-o", monitor, "-m", "fill", "-i", wallpaper]

  getShuffled monitors wallpapers = do
    baseSet <- cycle <$> shuffleM wallpapers
    shuffleM $ zipWith const baseSet monitors

  getWallpapers :: String -> IO [String]
  getWallpapers source = do
    isDir <- doesDirectoryExist source
    if isDir
      then foldMap (getWallpapers . (source </>)) =<< listDirectory source
      else pure [source]

  getLocation :: FilePath -> String -> IO (Maybe FilePath)
  getLocation base monitor = traverse canonicalizePath =<< findLocation ((base </>) <$> locations monitor)
  findLocation = foldr getLocation' (pure Nothing)
  getLocation' dir alt = bool alt (pure $ Just dir) =<< doesDirectoryExist dir
  locations monitor =
    [ "background-" ++ network ++ "=" ++ monitor
    , "background=" ++ monitor
    , "background-" ++ network
    , "background"
    ]

getMonitors :: IO [String]
getMonitors = do
  raw <- readProcess "hyprctl" ["monitors"] ""
  let monitors = toList . stripPrefix "Monitor " =<< lines raw
  pure $ takeWhile (not . isSpace) <$> monitors
