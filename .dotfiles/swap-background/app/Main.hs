{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-} -- see `pop` in `shuffle`

module Main where

import Control.Applicative (Alternative, (<|>))
import Control.Monad (join, mfilter, void, (<=<), (>=>))
import Control.Monad.Extra (findM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, evalState, evalStateT, mapStateT, runState, state)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Bool (bool)
import Data.Distributive (distribute)
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (uncons)
import Data.List.Extra (groupSortOn, stripPrefix, (!?))
import Data.List.NonEmpty (nonEmpty, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (secondM)
import System.Directory (doesDirectoryExist, findExecutable, getHomeDirectory, getSymbolicLinkTarget)
import System.Directory.Recursive (getFilesRecursive)
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>))
import System.Process (readProcess)
import System.Random (Random (randomR), RandomGen (split), initStdGen)
import Text.Read (readMaybe)

maybeToM :: MonadFail m => String -> Maybe a -> m a
maybeToM err = fail err `maybe` pure

try :: Alternative f => (a -> f a) -> a -> f a
try f x = f x <|> pure x

type NetworkId = String
type SSID = String

data Output = Output
  { name :: String
  , resolution :: String
  }
  deriving (Show)

getOutputs :: IO [Output]
getOutputs = traverse parse . lines =<< shell "swww" ["query"] ""
 where
  parse = parse' . second (break (== ',')) . break (== ':')
  parse' (name, (':' : ' ' : resolution, _)) = pure $ Output name resolution
  parse' _ = fail "Could not parse `swww query`"

main :: IO ()
main = join $ setWallpapers <$> backgrounds <*> ssid'' <*> getOutputs
 where
  ssid'' = networkId >>= maybe ssidViaQuery ssidByNId

  -- When run via `wpa_cli -a`
  ssid' = (const . pure) offline `bool` getSsid
  ssidByNId = join <$> distribute (ssid' <$> connected)
  connected = (== Just "CONNECTED") . (!? 1) <$> getArgs
  networkId = mfilter (not . null) <$> lookupEnv "WPA_ID"

  -- When run to initially set wallpaper
  ssidViaQuery = fromMaybe offline . only . (=<<) (toList . stripPrefix "ssid=") . lines <$> shell "wpa_cli" ["status"] ""
  only = fmap fst . mfilter (null . snd) . uncons

  offline = "offline"

  backgrounds = (</> "Desktop") <$> getHomeDirectory

getSsid :: NetworkId -> IO SSID
getSsid networkId =
  shell "wpa_cli" ["get_network", networkId, "ssid"] ""
    >>= maybeToM "Could not deduce SSID"
      . (readMaybe <=< (!? 1))
      . lines

shell :: String -> [String] -> String -> IO String
shell bin args stdin = pathToBin >>= run
 where
  run binPath = readProcess binPath args stdin
  pathToBin = findExecutable bin >>= maybeToM ("Could not find executable: " <> bin)

setWallpapers :: FilePath -> SSID -> [Output] -> IO ()
setWallpapers base ssid outputs =
  evalStateT setWallpapers' =<< initStdGen
 where
  setWallpapers' =
    void . traverse setWallpaperForOutput . join
      <=< traverse pairOutputWallpaper . (toList =<<)
      <=< lift
      $ traverse handleMissingWallpaper
        =<< (traverse . traverse) getFilesRecursive'
          . groupByFilePath
        =<< toSndM subDirs `traverse` outputs
  pairOutputWallpaper =
    mapStateT (pure . runIdentity)
      . fmap (uncurry zip)
      . secondM (shuffleInf . toList)
  handleMissingWallpaper (paired, Just path) = pure $ Just (paired, path)
  handleMissingWallpaper (paired, Nothing) = traverse setToFallbackColor paired $> Nothing
  setWallpaperForOutput (output, wallpaper) = lift $ setBackground output wallpaper
  setToFallbackColor output = void $ shell "swww" ["clear", "-o", name output, "--", "0F0F0F"] ""
  setBackground output background =
    void $ shell "swww" ["img", "-o", name output, "-t", "none", "--", background] ""
  getFilesRecursive' maybeFiles = join <$> traverse (fmap nonEmpty . getFilesRecursive) maybeFiles
  toSndM f = fmap <$> (,) <*> f
  subDirs (Output{name}) =
    findDirAndFollowSymLink
      [ base </> "background-" <> ssid <> "=" <> name
      , base </> "background=" <> name
      , base </> "background-" <> ssid
      , base </> "background"
      ]
  groupByFilePath = fmap ((,) <$> fmap fst <*> snd . head) . groupSortOn snd
  findDirAndFollowSymLink = findM doesDirectoryExist >=> try (traverse getSymbolicLinkTarget)

shuffleInf :: RandomGen g => [a] -> State g [a]
shuffleInf xs = state $ first shuffleInf' . split
 where
  shuffleInf' = join . toList . unfoldr (second pure . shuffle' xs)
  shuffle' = runState . shuffle

shuffle :: RandomGen g => [a] -> State g [a]
shuffle [] = pure []
shuffle [x] = pure [x]
shuffle xs =
  fmap ((`evalState` xs) . traverse (state . pop))
    . traverse (state . randomR . (,) 0)
    . scanr (+) 0
    $ replicate (length xs - 1) 1
 where
  -- NOTE: errors when n >= length ys (never the case)
  pop n ys = let (pre, e : post) = splitAt n ys in (e, pre <> post)
