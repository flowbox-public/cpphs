{-
-- The main program for cpphs, a simple C pre-processor written in Haskell.

-- Copyright (c) 2004 Malcolm Wallace
-- This file is GPL, although the libraries it uses are either standard
-- Haskell'98 or distributed under the LGPL.
-}
module RunCpphs ( runCpphs ) where
import System   (exitWith, ExitCode(..))
import List     (isPrefixOf)
import Monad    (when)
import IO       (stdout, IOMode(WriteMode), openFile, hPutStr, hFlush)

import CppIfdef (cppIfdef)
import MacroPass(macroPass)

version :: String
version = "0.9"

runCpphs :: String -> [String] -> IO ()
runCpphs prog args = do
  let ds    = map (preDefine . drop 2) (filter ("-D"`isPrefixOf`) args)
      os    = map (drop 2) (filter ("-O"`isPrefixOf`) args)
      is    = map (trail "/\\" . drop 2) (filter ("-I"`isPrefixOf`) args)
      macro = not ("--nomacro" `elem` args)
      locat = not ("--noline" `elem` args)
      lang  = not ("--text" `elem` args)
      strip =      "--strip" `elem` args
      ansi  =      "--hashes" `elem` args
      layout=      "--layout" `elem` args
      files = filter (not . isPrefixOf "-") args
  when ("--version" `elem` args)
       (do putStrLn (prog++" "++version)
           exitWith ExitSuccess)
  when ("--help" `elem` args || length os > 1)
       (do putStrLn ("Usage: "++prog
                ++" [file ...] [ -Dsym | -Dsym=val | -Ipath ]*  [-Ofile]\n"
                ++"\t\t[--nomacro] [--noline] [--text]"
                ++" [--strip] [--hashes] [--layout]")
           exitWith (ExitFailure 1))
  o <- if null os then return stdout else openFile (head os) WriteMode
  mapM_ (\(f,action)->
              do c <- action
                 let pass1 = cppIfdef f ds is macro locat c
                     pass2 = macroPass ds strip ansi layout lang pass1
                 if not macro then hPutStr o (unlines (map snd pass1))
                              else hPutStr o pass2
        ) (if null files then [("stdin",getContents)]
                         else map (\f->(f,readFile f)) files)
  hFlush o

-- | Parse the body of a @-D@ option: the default value is 1.
preDefine :: String -> (String, String)
preDefine defn = (s, if null d then "1" else tail d)
  where (s,d) = break (=='=') defn

trail :: (Eq a) => [a] -> [a] -> [a]
trail xs = reverse . dropWhile (`elem`xs) . reverse