#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}

import Development.Shake.Command
import Control.Arrow
import Control.Monad.State
import Data.Char
import Data.Foldable
import Text.Printf
import System.Directory
import Development.Shake.FilePath
import Data.Functor
import Data.Traversable
import Data.List

data Paths = Paths
  { pCodex :: FilePath
  , pSymbols :: FilePath
  , pDest :: FilePath
  }

main = do
  pCodex <- getXdgDirectory XdgData "dotfiles/codex.git"
  let pSymbols = "src/modules"
  pDest <- getXdgDirectory XdgData "fcitx5/data/quickphrase.d/typst"
  for ["sym", "emoji"] $ genSymbols Paths{..}

genSymbols ps which = do
  (Stdout ls) <- cmd "git" ["--git-dir=" ++ pCodex ps] "show" ["HEAD:" ++ pSymbols ps </> which <.> "txt"]
  let ls' = toList =<< evalState (mapM (online 0) $ lines ls) []
  writeFile (printf "%s-%s.mb" (pDest ps) which) . unlines . sort $ ls' <&> \(p,x) -> p ++ " " ++ [charP x]

online :: Int -> String -> State [String] (Maybe (String, String))
online n (' ':' ':cs) = do
  prefix <- gets (!! n)
  res <- online (n+1) cs
  pure $ first (prefix ++) <$> res
online _ ('/':'/':_) = pure Nothing
online _ ('@':_) = pure Nothing -- i.e. @deprecated
online n cs = do
  let  (prefix, sym) = span (/=' ') cs
  modify ((++ [prefix]) . take n)
  case drop 1 sym of
    "" -> pure Nothing
    x -> pure $ Just (prefix, x)

charP :: String -> Char
charP [x] = x
charP ('U':'+':x) = chr . read $ "0x" ++ x
charP x = error $ "Could not decode `"++x++"` as a char"
