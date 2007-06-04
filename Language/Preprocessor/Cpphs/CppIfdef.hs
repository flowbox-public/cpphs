-----------------------------------------------------------------------------
-- |
-- Module      :  CppIfdef
-- Copyright   :  1999-2004 Malcolm Wallace
-- Licence     :  LGPL
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Perform a cpp.first-pass, gathering \#define's and evaluating \#ifdef's.
-- and \#include's.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.CppIfdef
  ( cppIfdef	-- :: FilePath -> [(String,String)] -> [String] -> Options
		--      -> String -> [(Posn,String)]
  ) where


import Language.Preprocessor.Cpphs.SymTab
import Text.ParserCombinators.HuttonMeijer
-- import HashDefine
import Language.Preprocessor.Cpphs.Position  (Posn,newfile,newline,newlines
                                             ,cppline,newpos)
import Language.Preprocessor.Cpphs.ReadFirst (readFirst)
import Language.Preprocessor.Cpphs.Tokenise  (linesCpp,reslash)
import Language.Preprocessor.Cpphs.Options   (BoolOptions(..))
import Char      (isDigit)
import Numeric   (readHex,readOct,readDec)
import System.IO.Unsafe (unsafePerformIO)
import IO        (hPutStrLn,stderr)


-- | Run a first pass of cpp, evaluating \#ifdef's and processing \#include's,
--   whilst taking account of \#define's and \#undef's as we encounter them.
cppIfdef :: FilePath		-- ^ File for error reports
	-> [(String,String)]	-- ^ Pre-defined symbols and their values
	-> [String]		-- ^ Search path for \#includes
	-> BoolOptions		-- ^ Options controlling output style
	-> String		-- ^ The input file content
	-> [(Posn,String)]	-- ^ The file after processing (in lines)
cppIfdef fp syms search options =
    cpp posn defs search options Keep . (cppline posn:) . linesCpp
  where
    posn = newfile fp
    defs = foldr insertST emptyST syms
-- Notice that the symbol table is a very simple one mapping strings
-- to strings.  This pass does not need anything more elaborate, in
-- particular it is not required to deal with any parameterised macros.


-- | Internal state for whether lines are being kept or dropped.
--   In @Drop n b@, @n@ is the depth of nesting, @b@ is whether
--   we have already succeeded in keeping some lines in a chain of
--   @elif@'s
data KeepState = Keep | Drop Int Bool

-- | Return just the list of lines that the real cpp would decide to keep.
cpp :: Posn -> SymTab String -> [String] -> BoolOptions -> KeepState
       -> [String] -> [(Posn,String)]
cpp _ _ _ _ _ [] = []

cpp p syms path options Keep (l@('#':x):xs) =
    let ws = words x
        cmd = head ws
        line = tail ws
        sym  = head (tail ws)
        rest = tail (tail ws)
        val  = maybe "1" id (un rest)
        un v = if null v then Nothing else Just (unwords v)
        keepIf p = if p then Keep else (Drop 1 False)
        skipn syms' ud xs' =
            let n = 1 + length (filter (=='\n') l) in
            (if macros options then ((p,reslash l):)
                               else (replicate n (p,"") ++)) $
            cpp (newlines n p) syms' path options ud xs'
    in case cmd of
	"define" -> skipn (insertST (sym,val) syms) Keep xs
	"undef"  -> skipn (deleteST sym syms) Keep xs
	"ifndef" -> skipn syms (keepIf (not (definedST sym syms))) xs
	"ifdef"  -> skipn syms (keepIf      (definedST sym syms)) xs
	"if"     -> skipn syms (keepIf (gatherDefined p syms (unwords line))) xs
	"else"   -> skipn syms (Drop 1 False) xs
	"elif"   -> skipn syms (Drop 1 True) xs
	"endif"  -> skipn syms  Keep xs
	"pragma" -> skipn syms  Keep xs
        ('!':_)  -> skipn syms  Keep xs	-- \#!runhs scripts
	"include"-> let (inc,content) =
	                  unsafePerformIO (readFirst (unwords (tail ws))
                                                     p path syms
                                                     (warnings options))
	            in
		    cpp p syms path options Keep (("#line 1 "++show inc)
                                                  : linesCpp content
                                                  ++ cppline (newline p): xs)
	"warning"-> if warnings options then unsafePerformIO $ do
                       hPutStrLn stderr (l++"\nin "++show p)
                       return $ skipn syms Keep xs
                    else skipn syms Keep xs
	"error"  -> error (l++"\nin "++show p)
	"line"   | all isDigit sym
	         -> (if locations options then ((p,l):) else id) $
                    cpp (newpos (read sym) (un rest) p)
                        syms path options Keep xs
	n | all isDigit n
	         -> (if locations options then ((p,l):) else id) $
	            cpp (newpos (read n) (un (tail ws)) p)
                        syms path options Keep xs
          | otherwise
	         -> if warnings options then unsafePerformIO $ do
                       hPutStrLn stderr ("Warning: unknown directive #"++n
                                        ++"\nin "++show p)
                       return $
                         ((p,l): cpp (newline p) syms path options Keep xs)
                    else
                         ((p,l): cpp (newline p) syms path options Keep xs)

cpp p syms path options (Drop n b) (('#':x):xs) =
    let ws = words x
        cmd = head ws
        delse    | n==1 && b = Drop 1 b
                 | n==1      = Keep
                 | otherwise = Drop n b
        dend     | n==1      = Keep
                 | otherwise = Drop (n-1) b
        delif s  | n==1 && not b && gatherDefined p syms s
                             = Keep
                 | otherwise = Drop n b
        skipn ud xs' =
                 let n' = 1 + length (filter (=='\n') x) in
                 replicate n' (p,"")
                 ++ cpp (newlines n' p) syms path options ud xs'
    in
    if      cmd == "ifndef" ||
            cmd == "if"     ||
            cmd == "ifdef"  then  skipn (Drop (n+1) b) xs
    else if cmd == "elif"   then  skipn (delif (unwords (tail ws))) xs
    else if cmd == "else"   then  skipn  delse xs
    else if cmd == "endif"  then  skipn  dend  xs
    else skipn (Drop n b) xs
	-- define, undef, include, error, warning, pragma, line

cpp p syms path options Keep (x:xs) =
    let p' = newline p in seq p' $
    (p,x):  cpp p' syms path options Keep xs
cpp p syms path options d@(Drop _ _) (_:xs) =
    let p' = newline p in seq p' $
    (p,""): cpp p' syms path options d xs


----
gatherDefined :: Posn -> SymTab String -> String -> Bool
gatherDefined p st inp =
  case papply (parseBoolExp st) inp of
    []      -> error ("Cannot parse #if directive in file "++show p)
    [(b,_)] -> b
    _       -> error ("Ambiguous parse for #if directive in file "++show p)

parseBoolExp :: SymTab String -> Parser Bool
parseBoolExp st =
  do  a <- parseExp1 st
      skip (string "||")
      b <- first (skip (parseBoolExp st))
      return (a || b)
  +++
      parseExp1 st

parseExp1 :: SymTab String -> Parser Bool
parseExp1 st =
  do  a <- parseExp0 st
      skip (string "&&")
      b <- first (skip (parseExp1 st))
      return (a && b)
  +++
      parseExp0 st

parseExp0 :: SymTab String -> Parser Bool
parseExp0 st =
  do  skip (string "defined")
      sym <- bracket (skip (char '(')) (skip (many1 alphanum)) (skip (char ')'))
      return (definedST sym st)
  +++
  do  bracket (skip (char '(')) (parseBoolExp st) (skip (char ')'))
  +++
  do  skip (char '!')
      a <- parseExp0 st
      return (not a)
  +++
  do  sym1 <- skip (many1 alphanum)
      op <- parseOp st
      sym2 <- skip (many1 alphanum)
      let val1 = safeRead (convert sym1)
      let val2 = safeRead (convert sym2)
      return (op val1 val2)
  +++
  do  sym <- skip (many1 alphanum)
      case safeRead (convert sym) of
        0 -> return False
        _ -> return True
  where
    convert sym =
      case lookupST sym st of
        Nothing  -> sym
        (Just a) -> convert a
    safeRead s =
      case s of
        '0':'x':s' -> number readHex s'
        '0':'o':s' -> number readOct s'
        _          -> number readDec s
    number rd s =
      case rd s of
        []        -> 0 :: Integer
        ((n,_):_) -> n :: Integer

parseOp :: SymTab String -> Parser (Integer -> Integer -> Bool)
parseOp _ =
  do  skip (string ">=")
      return (>=)
  +++
  do  skip (char '>')
      return (>)
  +++
  do  skip (string "<=")
      return (<=)
  +++
  do  skip (char '<')
      return (<)
  +++
  do  skip (string "==")
      return (==)
  +++
  do  skip (string "!=")
      return (/=)
