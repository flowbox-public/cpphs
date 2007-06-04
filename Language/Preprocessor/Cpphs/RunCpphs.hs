{-
-- The main program for cpphs, a simple C pre-processor written in Haskell.

-- Copyright (c) 2004 Malcolm Wallace
-- This file is GPL, although the libraries it uses are either standard
-- Haskell'98 or distributed under the LGPL.
-}
module Language.Preprocessor.Cpphs.RunCpphs ( runCpphs ) where

import Language.Preprocessor.Cpphs.CppIfdef  (cppIfdef)
import Language.Preprocessor.Cpphs.MacroPass (macroPass)
import Language.Preprocessor.Cpphs.Options as Options
                                             (CpphsOptions(..), BoolOptions(..))
import Language.Preprocessor.Unlit as Unlit (unlit)


runCpphs :: CpphsOptions -> FilePath -> String -> String
runCpphs options filename input =
  let bools = boolopts options

      pass1 = cppIfdef filename (defines options) (includes options) bools input
      pass2 = macroPass (defines options) bools pass1
      result= if not (macros bools) then unlines (map snd pass1) else pass2
      pass3 = if Options.unlit bools then Unlit.unlit filename else id

  in pass3 result
