{-
-- The main program for cpphs, a simple C pre-processor written in Haskell.

-- Copyright (c) 2004 Malcolm Wallace
-- This file is GPL, although the libraries it uses are either standard
-- Haskell'98 or distributed under the LGPL.
-}
module Main where
import System   (getArgs, getProgName, exitWith, ExitCode(..))
import List     (isPrefixOf)
import Monad    (when)
import IO       (stdout, IOMode(WriteMode), openFile, hPutStr)

import CppIfdef (cppIfdef, preDefine)
import Position (newfile)
import MacroPass(macroPass)

version :: String
version = "0.5"

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  runCpphs prog args

runCpphs :: String -> [String] -> IO ()
runCpphs prog args = do
  let ds    = map (drop 2) (filter ("-D"`isPrefixOf`) args)
      os    = map (drop 2) (filter ("-O"`isPrefixOf`) args)
      is    = map (trail "/\\" . drop 2) (filter ("-I"`isPrefixOf`) args)
      macro = not ("--nomacro" `elem` args)
      locat = not ("--noline" `elem` args)
      strip =      "--strip" `elem` args
      ansi  =      "--hashes" `elem` args
      layout=      "--layout" `elem` args
      files = filter (not . isPrefixOf "-") args
  when ("--version" `elem` args)
       (do putStrLn (prog++" "++version)
           exitWith ExitSuccess)
  when (null files || length os > 1)
       (do putStrLn ("Usage: "++prog
                ++" file ... [ -Dsym | -Dsym=val | -Ipath ]*  [-Ofile]\n"
                ++"\t\t[--nomacro] [--noline] [--strip] [--hashes] [--layout]")
           exitWith (ExitFailure 1))
  o <- if null os then return stdout else openFile (head os) WriteMode
  mapM_ (\f-> do c <- readFile f
                 let pass1 = cppIfdef (newfile f) (preDefine ds)
                                      is macro locat c
                     pass2 = macroPass ds strip ansi layout pass1
                 if not macro then hPutStr o pass1 else hPutStr o pass2
        ) files

trail :: (Eq a) => [a] -> [a] -> [a]
trail xs = reverse . dropWhile (`elem`xs) . reverse
