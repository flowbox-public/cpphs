{-
-- A test module for cpphs

-- Copyright (c) 2004 Graham Klyne
-}
module CpphsTest where

import Cpphs( runCpphs )
import IO
    ( Handle, stdout, IOMode(WriteMode)
    , openFile, hClose, hPutStr, hPutStrLn )
import HUnit
    ( Test(TestCase,TestList,TestLabel)
    , assertEqual, runTestTT, runTestText, putTextToHandle )


runCpphsTest :: [String] -> String -> String -> Test
runCpphsTest args result expect = TestCase $ do
  runCpphs "cpphs" (("-O"++result):args)
  res <- readFile result
  exp <- readFile expect
  assertEqual ("cpphs "++concatMap (' ':) args) exp res

test1 = runCpphsTest ["-Itests/","--nomacro","tests/testfile"]
                     "tests/resultfile" "tests/expectfile1"
test2 = runCpphsTest ["-Itests/","--nomacro","-Dnoelif","tests/testfile"]
                     "tests/resultfile" "tests/expectfile2"
test3 = runCpphsTest ["-Itests/","--nomacro","-Delif","tests/testfile"]
                     "tests/resultfile" "tests/expectfile3"
test4 = runCpphsTest ["-Itests/","--nomacro","-Dinclude","tests/testfile"]
                     "tests/resultfile" "tests/expectfile4"
test5 = runCpphsTest ["-Itests/","--noline","-Dinclude","tests/testfile"]
                     "tests/resultfile" "tests/expectfile5"
test6 = runCpphsTest ["-Itests/","tests/cpp"]
                     "tests/resultfile" "tests/expectfile6"
test7 = runCpphsTest ["-Itests/","-D__GLASGOW_HASKELL__","tests/Storable.hs"]
                     "tests/resultfile" "tests/expectfile7"
test8 = runCpphsTest ["-Itests/","-DCALLCONV=ccall","tests/HsOpenGLExt.h"]
                     "tests/resultfile" "tests/expectfile8"
test9 = runCpphsTest ["-Itests/","tests/multiline"]
                     "tests/resultfile" "tests/expectfile9"
test10 = runCpphsTest ["-Itests/","--nomacro","tests/multiline"]
                     "tests/resultfile" "tests/expectfile10"

allTests = TestList
    [ test1
    , test2
    , test3
    , test4
    , test5
    , test6
    , test7
    , test8
    , test9
    , test10
    ]

run t = runTestTT t

--  To run  run allTests
