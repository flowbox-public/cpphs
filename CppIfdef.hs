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
-- Perform a cpp.first-pass, gathering #define's and evaluating #ifdef's.
-- and #include's.
-----------------------------------------------------------------------------

module CppIfdef
  ( cppIfdef	-- :: Posn -> SymTab String -> String -> String
  , preDefine	-- :: [String] -> SymTab String
  ) where

import SymTab
import ParseLib
-- import HashDefine
import Position    (Posn,newline,newlines,cppline,hashline)
import ReadFirst   (readFirst)
import Tokenise    (linesCpp,reslash)
import Char        (isDigit)
import Numeric     (readHex)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

-- | Run a first pass of cpp, evaluating #ifdef's and processing #include's,
--   whilst taking account of #define's and #undef's as we encounter them.
cppIfdef :: Posn		-- ^ Position info for error reports
	-> SymTab String	-- ^ Pre-defined symbols
	-> [String]		-- ^ Search path for #includes
	-> Bool			-- ^ Leave #define and #undef in output?
	-> Bool			-- ^ Place #line droppings in output?
	-> String		-- ^ The input file content
	-> String		-- ^ The file after processing
cppIfdef posn syms search leave locat =
    unlines . cpp posn syms search leave locat Keep . (cppline posn:) . linesCpp
-- Notice that the symbol table is a very simple one mapping strings
-- to strings.  This pass does not need anything more elaborate, in
-- particular it is not required to deal with any parameterised macros.


-- | Command-line definitions via -D are parsed here.
preDefine :: [String] -> SymTab String
preDefine defines =
    foldr (insertST.defval) emptyST defines
  where
    defval sym = let (s,d) = break (=='=') sym
                 in (s, if null d then "1" else tail d)


-- | Internal state for whether lines are being kept or dropped.
--   In @Drop n b@, @n@ is the depth of nesting, @b@ is whether
--   we have already succeeded in keeping some lines in a chain of
--   @elif@'s
data KeepState = Keep | Drop Int Bool

-- | Return just the list of lines that the real cpp would decide to keep.
cpp :: Posn -> SymTab String -> [String] -> Bool -> Bool -> KeepState
       -> [String] -> [String]
cpp _ _ _ _ _ _ [] = []

cpp p syms path leave ln Keep (l@('#':x):xs) =
    let ws = words x
        cmd = head ws
        sym = head (tail ws)
        val = maybe "1" id rest
        rest = let v = tail (tail ws) in
               if null v then Nothing else Just (unwords v)
        down = if definedST sym syms then (Drop 1 False) else Keep
        up   = if definedST sym syms then Keep else (Drop 1 False)
        keep str = if gatherDefined p syms str then Keep else (Drop 1 False)
        skipn cpp p syms path ud xs =
            let n = 1 + length (filter (=='\n') l) in
            (if leave then (reslash l:) else (replicate n "" ++)) $
            cpp (newlines n p) syms path leave ln ud xs
    in case cmd of
	"define" -> skipn cpp p (insertST (sym,val) syms) path Keep xs
	"undef"  -> skipn cpp p (deleteST sym syms) path Keep xs
	"ifndef" -> skipn cpp p syms path  down xs
	"ifdef"  -> skipn cpp p syms path  up   xs
	"if"     -> skipn cpp p syms path (keep (unwords (tail ws))) xs
	"else"   -> skipn cpp p syms path (Drop 1 False) xs
	"elif"   -> skipn cpp p syms path (Drop 1 True) xs
	"endif"  -> skipn cpp p syms path  Keep xs
	"include"-> let (inc,content) =
	                  unsafePerformIO (readFirst (unwords (tail ws))
                                                     p path syms)
	            in
		    cpp p syms path leave ln Keep (("#line 1 "++show inc)
                                                  : linesCpp content
                                                  ++ cppline p :"": xs)
	"warning"-> trace (l++"\nin "++show p) $
	            skipn cpp p syms path  Keep xs
	"error"  ->  error (l++"\nin "++show p)
	"line" | all isDigit sym
	         -> (if ln then l else ""):
                    cpp (hashline (read sym) rest p) syms path leave ln Keep xs
	n | all isDigit n
	         -> (if ln then l else ""):
	            cpp (hashline (read n) Nothing p) syms path leave ln Keep xs
          | otherwise
	         -> error ("Unknown directive #"++cmd++"\nin "++show p)

cpp p syms path leave ln (Drop n b) (('#':x):xs) =
    let ws = words x
        cmd = head ws
        delse    | n==1 && b = Drop 1 b
                 | n==1      = Keep
                 | otherwise = Drop n b
        dend     | n==1      = Keep
                 | otherwise = Drop (n-1) b
        keep str | n==1      = if gatherDefined p syms str then Keep
                               else (Drop 1) b
                 | otherwise = Drop n b
        skipn cpp p syms path ud xs =
                 let n = 1 + length (filter (=='\n') x) in
                 replicate n "" ++ cpp (newlines n p) syms path leave ln ud xs
    in
    if      cmd == "define"  ||
            cmd == "undef"   ||
            cmd == "error"   ||
            cmd == "include" ||
            cmd == "warning" ||
            cmd == "line"   then  skipn cpp p syms path (Drop n b) xs
    else if cmd == "ifndef" ||
            cmd == "if"     ||
            cmd == "ifdef"  then  skipn cpp p syms path (Drop (n+1) b) xs
    else if cmd == "elif"   then  skipn cpp p syms path
                                                  (keep (unwords (tail ws))) xs
    else if cmd == "else"   then  skipn cpp p syms path delse xs
    else if cmd == "endif"  then  skipn cpp p syms path dend xs
    else skipn cpp p syms path (Drop n b) xs

cpp p syms path leave ln Keep (x:xs) =
    x:  cpp (newline p) syms path leave ln Keep xs
cpp p syms path leave ln d@(Drop n b) (x:xs) =
    "": cpp (newline p) syms path leave ln d xs


----
gatherDefined p st inp =
  case papply (parseBoolExp st) inp of
    []      -> error ("Cannot parse #if directive in file "++show p)
    [(b,_)] -> b
    _       -> error ("Ambiguous parse for #if directive in file "++show p)

parseBoolExp st =
  do  bracket (skip (char '(')) (parseBoolExp st) (skip (char ')'))
  +++
  do  skip (char '!')
      a <- skip (parseSym st)		-- deals with !x && y
      parseCont (not a) st
  +++
  do  skip (char '!')
      a <- parseBoolExp st		-- deals with !(x && y)
      parseCont (not a) st
  +++
  do  a <- skip (parseSym st)
      parseCont a st

parseSym st =
  do  skip (string "defined")
      sym <- bracket (skip (char '(')) (skip (many1 alphanum)) (skip (char ')'))
      return (definedST sym st)
  +++
  do  sym <- skip (many1 alphanum)
      parseComparison sym st

parseCont a st =
  do  skip (string "||")
      b <- first (skip (parseBoolExp st))
      return (a || b)
  +++
  do  skip (string "&&")
      b <- first (skip (parseBoolExp st))
      return (a && b)
  +++
  do  return a

parseComparison sym1 st =
  do  op <- parseOp st
      sym2 <- skip (many1 alphanum)
      let val1 = convert sym1 st
      let val2 = convert sym2 st
      return (op val1 val2)
  +++
  do  let val = lookupST sym1 st
      return (if val == Nothing || val == Just "0" then False else True)
  where
    convert sym st =
      case lookupST sym st of
        Nothing  -> safeRead sym
        (Just a) -> safeRead a
    safeRead s =
      case readHex s of
        []        -> 0 :: Integer
        ((n,_):_) -> n :: Integer

parseOp st =
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

----
