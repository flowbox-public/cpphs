-----------------------------------------------------------------------------
-- |
-- Module      :  Tokenise
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- The purpose of this module is to lex a source file (language
-- unspecified) into tokens such that cpp can recognise a replaceable
-- symbol or macro-use, and do the right thing.
-----------------------------------------------------------------------------

module Tokenise
  ( linesCpp
  , reslash
  , tokenise
  , WordStyle(..)
  , deWordStyle
  , parseMacroCall
  ) where

import Char
import HashDefine

-- | A Mode value describes whether to tokenise a la Haskell, or a la Cpp.
--   The main difference is that in Cpp mode we should recognise line
--   continuation characters.
data Mode = Haskell | Cpp

-- | linesCpp is, broadly speaking, Prelude.lines, except that
--   on a line beginning with a #, line continuation characters are
--   recognised.  In a continuation, the newline character is
--   preserved, but the backslash is not.
linesCpp :: String -> [String]
linesCpp  []                 = []
linesCpp (x:xs) | x=='#'     = tok Cpp     ['#'] xs
                | otherwise  = tok Haskell [] (x:xs)
  where
    tok Cpp   acc ('\\':'\n':xs)   = tok Cpp ('\n':acc) xs
    tok _     acc ('\n':'#':xs)    = reverse acc: tok Cpp ['#'] xs
    tok _     acc ('\n':xs)        = reverse acc: tok Haskell [] xs
    tok _     acc []               = reverse acc: []
    tok mode  acc (x:xs)           = tok mode (x:acc) xs

-- | Put back the line-continuation characters.
reslash :: String -> String
reslash ('\n':xs) = '\\':'\n':reslash xs
reslash (x:xs)    = x: reslash xs
reslash   []      = []

----
-- | Submodes are required to deal correctly with nesting of lexical
--   structures.
data SubMode = Any | Pred (Char->Bool) (String->WordStyle)
             | String Char | LineComment | NestComment Int
             | Hash

-- | Each token is classified as one of Ident, Other, or Cmd
--   * Ident is a word that could potentially match a macro name.
--   * Cmd is a complete cpp directive (#define etc).
--   * Other is anything else.
data WordStyle = Ident String | Other String | Cmd (Maybe HashDefine)
  deriving (Eq,Show)

deWordStyle (Ident i) = i
deWordStyle (Other i) = i
deWordStyle (Cmd i)   = "\n"

-- | tokenise is, broadly-speaking, Prelude.words, except that:
--    * each word-like "token" is categorised as one of {Ident,Other,Cmd}
--    * #define's are parsed and returned out-of-band using the Cmd variant
--    * All whitespace is preserved intact as tokens.
--    * C-comments are converted to white-space.
--    * Parens and commas are tokens in their own right.
--    * The cpp ## catenation operator is stripped.
--    * Any cpp line continuations are respected.
--   No errors can be raised.
--   The inverse of tokenise is (concatMap deWordStyle).
tokenise :: String -> [WordStyle]
--tokenise :: String -> [Either String HashDefine]
tokenise = haskell Any []
  where
    -- rules to lex Haskell
    haskell :: SubMode -> String -> String -> [WordStyle]
    haskell Any acc ('\n':'#':xs)      = emit acc $  -- emit "\n" $
                                         cpp Any [] [] xs
						-- warning: non-maximal munch
    haskell Any acc ('-':'-':xs)       = emit acc $
                                         haskell LineComment "--" xs
    haskell Any acc ('{':'-':xs)       = emit acc $
                                         haskell (NestComment 0) "-{" xs
    haskell Any acc ('"':xs)           = emit acc $
                                         haskell (String '"') ['"'] xs
    haskell Any acc ('\'':xs)          = emit acc $
                                         haskell (String '\'') "'" xs
    haskell Any acc (x:xs) | single x  = emit acc $ emit [x] $
                                         haskell Any [] xs
    haskell Any acc (x:xs) | space x   = emit acc $
                                         haskell (Pred space Other) [x] xs
    haskell Any acc (x:xs) | symbol x  = emit acc $
                                         haskell (Pred symbol Other) [x] xs
 -- haskell Any []  (x:xs) | ident0 x  = id $
    haskell Any acc (x:xs) | ident0 x  = emit acc $
                                         haskell (Pred ident1 Ident) [x] xs
    haskell Any acc (x:xs)             = haskell Any (x:acc) xs

    haskell pred@(Pred p ws) acc (x:xs)
                           | p x       = haskell pred (x:acc) xs
                           | otherwise = ws (reverse acc): haskell Any [] (x:xs)
    haskell pred@(Pred p ws) acc []    = ws (reverse acc): []
    haskell (String c) acc ('\\':x:xs)
                           | x=='\\'   = haskell (String c) ('\\':'\\':acc) xs
                           | x==c      = haskell (String c) (c:'\\':acc) xs
    haskell (String c) acc (x:xs)
                           | x==c      = emit (c:acc) $ haskell Any [] xs
                           | otherwise = haskell (String c) (x:acc) xs
    haskell LineComment acc xs@('\n':_)= emit acc $ haskell Any [] xs
    haskell LineComment acc (x:xs)     = haskell LineComment (x:acc) xs
    haskell (NestComment n) acc ('{':'-':xs)
                                       = haskell (NestComment (n+1))
                                                                 ("-{"++acc) xs
    haskell (NestComment 0) acc ('-':'}':xs)
                                       = emit ("}-"++acc) $
                                         haskell Any [] xs
    haskell (NestComment n) acc ('-':'}':xs)
                                       = haskell (NestComment (n-1))
                                                                 ("}-"++acc) xs
    haskell (NestComment n) acc (x:xs) = haskell (NestComment n) (x:acc) xs
    haskell _   acc []                 = emit acc $ []

    -- rules to lex Cpp
    cpp :: SubMode -> String -> [String] -> String -> [WordStyle]
    cpp Any w l ('/':'*':xs)        = cpp (NestComment 0) "" (w*/*l) xs
    cpp Any w l ('/':'/':xs)        = cpp LineComment "  " (w*/*l) xs
    cpp Any w l ('\\':'\n':xs)      = cpp Any [] ("\n":w*/*l) xs
    cpp Any w l xs@('\n':_)         = Cmd (parseHashDefine (reverse (w*/*l))):
                                      haskell Any [] xs
 -- cpp Any w l ('"':xs)            = cpp (String '"') ['"'] (w*/*l) xs
 -- cpp Any w l ('\'':xs)           = cpp (String '\'') "'"  (w*/*l) xs
    cpp Any w l ('"':xs)            = cpp Any [] ("\"":(w*/*l)) xs
    cpp Any w l ('\'':xs)           = cpp Any [] ("'": (w*/*l)) xs
    cpp Any [] l (x:xs) | ident0 x  = cpp (Pred ident1 Ident) [x] l xs
 -- cpp Any w l (x:xs)  | ident0 x  = cpp (Pred ident1 Ident) [x] (w*/*l) xs
    cpp Any w l (x:xs)  | single x  = cpp Any [] ([x]:w*/*l) xs
                        | space x   = cpp (Pred space Other) [x] (w*/*l) xs
                        | symbol x  = cpp (Pred symbol  Other) [x] (w*/*l) xs
                        | otherwise = cpp Any (x:w) l xs

    cpp pred@(Pred p _) w l (x:xs)
                        | p x       = cpp pred (x:w) l xs
                        | otherwise = cpp Any [] (w*/*l) (x:xs)
    cpp pred@(Pred p _) w l []      = cpp Any [] (w*/*l) "\n"
    cpp (String c) w l ('\\':x:xs)
                        | x=='\\'   = cpp (String c) ('\\':'\\':w) l xs
                        | x==c      = cpp (String c) (c:'\\':w) l xs
    cpp (String c) w l (x:xs)
                        | x==c      = cpp Any [] ((c:w)*/*l) xs
                        | otherwise = cpp (String c) (x:w) l xs
    cpp LineComment w l ('\\':'\n':xs)
                                    = cpp LineComment [] (('\n':w)*/*l) xs
    cpp LineComment w l xs@('\n':_) = cpp Any w l xs
    cpp LineComment w l (x:xs)      = cpp LineComment (' ':w) l xs
    cpp (NestComment n) w l ('*':'/':xs)
                                    = cpp Any [] (w*/*l) xs
    cpp (NestComment n) w l (x:xs)  = cpp (NestComment n) (' ':w) l xs
    cpp _   w l []                  = []

    -- predicates for lexing Haskell.
    ident0 x = isAlpha x    || x `elem` "_`"
    ident1 x = isAlphaNum x || x `elem` "'_`"
    symbol x = x `elem` ":!#$%&*+./<=>?@\\^|-~"
    single x = x `elem` "(),[];{}"
    space  x = x `elem` " \t"
    -- emit a token (if there is one) from the accumulator
    emit ""  = id
    emit xs  = (Other (reverse xs):)
    -- add a reversed word to the accumulator
    "" */* l = l
    w */* l  = reverse w : l


-- | parse a possible macro call, returning argument list and remaining input
parseMacroCall :: [WordStyle] -> Maybe ([String],[WordStyle])
parseMacroCall = call . skip
  where
    skip xss@(Other x:xs) | all isSpace x = skip xs
    skip xss              | otherwise     = xss
    call (Other "(":xs)   = (args (0::Int) [] [] . skip) xs
    call _                = Nothing
    args 0 w acc (Other ",":xs) = args 0 [] (addone w acc) (skip xs)
    args n w acc (Other "(":xs) = args (n+1) ("(":w) acc xs
    args 0 w acc (Other ")":xs) = Just (reverse (addone w acc), xs)
    args n w acc (Other ")":xs) = args (n-1) (")":w) acc xs
    args n w acc (Ident var:xs) = args n (var:w) acc xs
    args n w acc (Other var:xs) = args n (var:w) acc xs
    args n w acc _              = Nothing
    addone w acc = concat (reverse (dropWhile (all isSpace) w)): acc
