#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as N
import Development.Shake.Command
import System.Directory
import System.Exit
import System.FilePath

data Paths = Paths
  { pCodex :: FilePath
  , pTypst :: FilePath
  , pSymbols :: FilePath
  , pDest :: FilePath
  }

main :: IO ()
main = do
  pCodex <- getXdgDirectory XdgData "dotfiles/codex.git"
  pTypst <- getXdgDirectory XdgData "dotfiles/typst.git"
  let pSymbols = "src/modules"
  pDest <- getXdgDirectory XdgData "fcitx5/data/quickphrase.d/typst"
  for_ ["sym", "emoji"] $ genSymbols Paths{..}

  genShorthand Paths{..}

genShorthand :: Paths -> IO ()
genShorthand ps = do
  (Stdout ls, Exit ExitSuccess) <-
    cmd "git" ["--git-dir=" ++ pTypst ps] "show HEAD:crates/typst-syntax/src/ast.rs"
  writeFcitx5 [pDest ps, "shorthand"] $ shorthandP ls

genSymbols :: Paths -> String -> IO ()
genSymbols ps which = do
  (Stdout ls, Exit ExitSuccess) <-
    cmd "git" ["--git-dir=" ++ pCodex ps] "show" ["HEAD:" ++ pSymbols ps </> which <.> "txt"]
  writeFcitx5 [pDest ps, which] $ codexP ls

writeFcitx5 :: [String] -> [(String, Char)] -> IO ()
writeFcitx5 names =
  writeFile (intercalate "-" names <.> "mb")
    . unlines
    . (take 1 <=< group)
    . sort
    . map (\(p, x) -> p ++ " " ++ [x])

shorthandP :: String -> [(String, Char)]
shorthandP =
  map shorthandLP
    . join
    . takeWhile (not . null)
    . unfoldr go
    . lines
 where
  go =
    Just
      . break (isSuffixOf "];")
      . drop 1
      . dropWhile (not . isInfixOf "pub const LIST")

shorthandLP :: String -> (String, Char)
shorthandLP =
  (extract '"' *** charP . extract '\'')
    . break (== ',')
 where
  extract c = takeWhile (/= c) . drop 1 . dropWhile (/= c)

codexP :: String -> [(String, Char)]
codexP ls = toList . snd =<< scanl (codexLP . fst) ([], Nothing) (lines ls)

codexLP :: [String] -> String -> ([String], Maybe (String, Char))
codexLP [] (' ' : ' ' : _) = error "First line is indented"
codexLP (p : ps) (' ' : ' ' : cs) = (p :) *** fmap (first (p ++)) $ codexLP ps cs
codexLP ps ('/' : '/' : _) = (ps, Nothing)
codexLP ps ('@' : _) = (ps, Nothing) -- i.e. @deprecated
codexLP _ cs =
  let
    (p, sym) = span (/= ' ') cs
    c = charP . toList <$> nonEmpty (dropWhile (== ' ') sym)
   in
    ([p], (p,) <$> c)

charP :: String -> Char
charP [x] = x
charP ('U' : '+' : x) = chr . read $ "0x" ++ x
charP ('\\' : 'u' : '{' : x : xs)
  | N.last (x :| xs) == '}' = chr . read $ "0x" ++ N.init (x :| xs)
charP x = error $ "Could not decode `" ++ x ++ "` as a char"
