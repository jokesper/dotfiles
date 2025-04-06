#!/usr/bin/env runhaskell

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MultiWayIf #-}

import Data.Foldable (toList)
import Data.List (stripPrefix)

import Control.Applicative (liftA2) -- compat with GHC <9.6
import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Control.Monad (unless, when)

import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (callCommand, readProcessWithExitCode)

main = singleInstance "/tmp/stop-and-shutdown.lock" waitForEnd *> callCommand "poweroff"
 where
  waitForEnd = do
    (exit, raw, _) <- readProcessWithExitCode "cmus-remote" ["-Q"] ""
    case exit of
      ExitSuccess -> whenCmus $ lines raw
      ExitFailure _ -> pure ()

  whenCmus raw = do
    status <- parseStatus "status " raw
    duration <- readIO =<< parseStatus "duration " raw
    position <- readIO =<< parseStatus "position " raw
    let remaining = duration - position

    if
      | status /= "playing" -> callCommand "cmus-remote -C quit"
      | remaining > 5 -> do
          threadDelay $ 5 * 10 ^ 6
          waitForEnd
      | otherwise -> do
          continue <- parseStatus "set continue " raw
          unless (continue `elem` ["false", "true"]) $
            fail $
              "Invalid value for `continue`: " ++ continue
          callCommand "cmus-remote -C 'set continue=0'"
          threadDelay $ remaining * 10 ^ 6
          callCommand $ "cmus-remote -C 'set continue=" ++ continue ++ "'"
          threadDelay $ 500 * 10 ^ 3
          callCommand "cmus-remote -C quit"
  singleInstance = liftA2 bracket_ aquireLock removeFile
  aquireLock lock = do
    shouldExit <- doesFileExist lock
    when shouldExit $ hPutStrLn stderr "Already running" *> exitFailure
    writeFile lock ""
  parseStatus p raw = extract p $ toList . stripPrefix p =<< raw
  extract p [] = fail $ "Missing statement from status: `" ++ p ++ "...`"
  extract _ [x] = pure x
  extract p (_ : _) = fail $ "Multiple statements from status: `" ++ p ++ "...`"
