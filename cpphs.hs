{-
-- The main program for cpphs, a simple C pre-processor written in Haskell.

-- Copyright (c) 2004 Malcolm Wallace
-- This file is placed in the public domain, but the library modules it
-- refers to are either standard Haskell'98, or distributed under the LGPL.
-}
module Main where
import System   (getArgs, getProgName)
import List     (isPrefixOf)
import Monad    (when)
import CppIfdef (cppIfdef, preDefine)
import Position (newfile)
import MacroPass(macroPass)

main = do
  args <- getArgs
  prog <- getProgName
  let ds    = map (drop 2) (filter ("-D"`isPrefixOf`) args)
      is    = map (drop 2) (filter ("-I"`isPrefixOf`) args)
      macro = not ("--nomacro" `elem` args)
      locat = not ("--noline" `elem` args)
      files = filter (not . isPrefixOf "-") args
  when (null files)
       (error ("Usage: "++prog++" file ... [-Dsym]* |-Dsym=val]* [-Ipath]*"
                              ++" [--nomacro] [--noline]"))
  mapM_ (\f-> do c <- readFile f
                 let pass1 = cppIfdef (newfile f) (preDefine ds)
                                      is macro locat c
                     pass2 = macroPass ds pass1
                 if not macro then putStr pass1 else putStr pass2
        ) files
