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
import Tokenise

main = do
  args <- getArgs
  prog <- getProgName
  let ds    = map (drop 2) (filter ("-D"`isPrefixOf`) args)
      is    = map (drop 2) (filter ("-I"`isPrefixOf`) args)
      files = filter (not . isPrefixOf "-") args
  when (null files)
       (error ("Usage: "++prog++" file ... [-Dsym]* |-Dsym=val]* [-Ipath]*"))
  mapM_ (\f-> do c <- readFile f
                 mapM_ print (tokenise c)
                 (putStr . concatMap deWordStyle . tokenise) c)
        files
