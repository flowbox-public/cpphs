{-
-- The main program for cpphs, a simple C pre-processor written in Haskell.

-- Copyright (c) 2004 Malcolm Wallace
-- This file is GPL, although the libraries it uses are either standard
-- Haskell'98 or distributed under the LGPL.
-}
module Main where

import System ( getArgs, getProgName )
import RunCpphs  ( runCpphs )

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  runCpphs prog args
