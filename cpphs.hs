{-
-- The main program wrapper for cpphs, a simple C pre-processor
-- written in Haskell.

-- Author: Malcolm Wallace, 2004
-- This file is placed into the public domain, as it is so trivial that it
-- probably is unprotectable.  Note however, that it uses modules 'RunCpphs',
-- which is GPL, and 'System' which is Haskell'98.  All other modules used
-- in turn by those are either distributed under the LGPL, or are Haskell'98.
--
-- Thus, when compiled as a standalone executable, this program will fall
-- under the GPL.
-}
module Main where

import System ( getArgs, getProgName )
import RunCpphs  ( runCpphs )

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  runCpphs prog args
