{-
-- The main program for cpphs, a simple C pre-processor written in Haskell.

-- Copyright (c) 2004 Malcolm Wallace
-- This file is placed in the public domain, but the library modules it
-- refers to are either standard Haskell'98, or distributed under the LGPL.
-}
module Main where
import System   (getArgs, getProgName)
import List     (isPrefixOf)
import Monad    (when,unless)
import CppIfdef (cppIfdef, preDefine)
import Position (newfile)
import IO
    ( Handle, stdout, IOMode(WriteMode)
    , openFile, hClose, hPutStr, hPutStrLn )
import HUnit
    ( Test(TestCase,TestList,TestLabel)
    , assertEqual, runTestTT, runTestText, putTextToHandle )

main = do
  args <- getArgs
  prog <- getProgName
  runCpphs prog args

runCpphs :: String -> [String] -> IO ()
runCpphs prog args =
  let ds    = map (drop 2) (filter ("-D"`isPrefixOf`) args)
      is    = map (drop 2) (filter ("-I"`isPrefixOf`) args)
      os    = map (drop 2) (filter ("-O"`isPrefixOf`) args)
      files = filter (not . isPrefixOf "-") args
  in do
      when (null files)
           (error ("Usage: "++prog++
                   " file ... [-Dsym]* |-Dsym=val]* [-Ipath]* [-Opath]"))
      oh <- if null os then return stdout else openFile (head os) WriteMode
      mapM_ (\f-> do c <- readFile f
                     hPutStr oh (cppIfdef (newfile f) (preDefine ds) is c))
            files
      unless (null os) (hClose oh)

runCpphsTest :: [String] -> String -> String -> Test
runCpphsTest args result expect = TestCase $ do
  runCpphs "cpphs" (("-O"++result):args)
  res <- readFile result
  exp <- readFile expect
  assertEqual ("cpphs "++concatMap (' ':) args) exp res

test1 = runCpphsTest ["-Itests/","tests/testfile"]
                     "tests/resultfile" "tests/expectfile1"
test2 = runCpphsTest ["-Itests/","-Dnoelif","tests/testfile"]
                     "tests/resultfile" "tests/expectfile2"
test3 = runCpphsTest ["-Itests/","-Dinclude","tests/testfile"]
                     "tests/resultfile" "tests/expectfile3"

allTests = TestList
    [ test1
    , test2
    , test3
    ]

run t = runTestTT t
