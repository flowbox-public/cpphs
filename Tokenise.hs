-- The purpose of this module is to lex a source file (language
-- unspecified) into tokens such that cpp can recognise a replaceable
-- symbol or macro-use, and do the right thing.
module Tokenise
  ( linesCpp
  , tokenise
  , deWordStyle
  ) where

import Char
import HashDefine

-- | A Mode value describes whether to tokenise a la Haskell, or a la Cpp.
--   The main difference is that in Cpp mode we should recognise line
--   continuation characters.
data Mode = Haskell | Cpp

-- linesCpp is, broadly speaking, Prelude.lines, except that
-- on a line beginning with a #, line continuation characters are
-- recognised.  In a continuation, the newline character is
-- preserved, but the backslash is not.
linesCpp :: String -> [String]
linesCpp (x:xs) | x=='#'     = tok Cpp     ['#'] xs
                | otherwise  = tok Haskell [] (x:xs)
  where
    tok Cpp   acc ('\\':'\n':xs)   = tok Cpp ('\n':acc) xs
    tok _     acc ('\n':'#':xs)    = reverse acc: tok Cpp ['#'] xs
    tok _     acc ('\n':xs)        = reverse acc: tok Haskell [] xs
    tok _     acc []               = reverse acc: []
    tok mode  acc (x:xs)           = tok mode (x:acc) xs

{-
-- | Submodes are required to deal correctly with nesting of lexical
--   structures.
data SubMode = Plain | String | Comment

-- tokenise is, broadly-speaking, Prelude.words, except that:
--  * All whitespace is preserved intact as tokens.
--  * C-comments are converted to white-space.
--  * Parens and commas are tokens in their own right.
--  * The cpp ## catenation operator is stripped.
--  * Any cpp line continuations are respected.
-- No errors can be raised.  The inverse of tokenise is concat.
tokenise :: String -> [String]
tokenise = tok Haskell Plain
  where
    tok Haskell Plain ('\n':'#':xs)    = "\n": "#": tok Cpp Plain xs
    tok Cpp     Plain ('\\':'\n':xs)   = tok Cpp Plain xs
    tok Cpp     Plain ('#':'#':xs)     = tok Cpp Plain xs
    tok _       Plain ('\n':xs)        = "\n": tok Haskell Plain xs
    tok mode    Plain ('(':xs)         = "(":  tok mode Plain xs
    tok mode    Plain (')':xs)         = ")":  tok mode Plain xs
    tok mode    Plain (',':xs)         = ",":  tok mode Plain xs
    tok mode    Plain ('/':'*':xs)     = ccomment mode "  " xs
    tok mode    Plain (x:xs) | space x = accumulate space [x] xs (tok mode Plain)
    tok mode    Plain (x:xs) | sym x   = accumulate sym [x] xs (tok mode Plain)
    tok _       Plain []               = []

    accumulate pred acc (x:xs) k | pred x = accumulate pred (x:acc) xs k
    accumulate pred acc xs     k          = reverse acc: k xs
    space x = x `elem` " \t"
    sym   x = not (x `elem` " \t\n(,)")

    ccomment mode acc ('*':'/':xs) = ("  "++reverse acc): tok mode Plain xs
    ccomment mode acc ('\n':xs)    = ccomment mode ('\n':acc) xs
    ccomment mode acc ('\t':xs)    = ccomment mode ('\t':acc) xs
    ccomment mode acc (x:xs)       = ccomment mode (' ':acc) xs
-}

----
-- | Submodes are required to deal correctly with nesting of lexical
--   structures.
data SubMode = Any | Pred (Char->Bool) (String->WordStyle)
             | String Char | LineComment | NestComment Int
             | Hash

data WordStyle = Ident String | Other String | Cmd (Maybe HashDefine)
  deriving (Eq,Show)

deWordStyle (Ident i) = i
deWordStyle (Other i) = i
deWordStyle (Cmd i)   = "\n"

-- tokenise is, broadly-speaking, Prelude.words, except that:
--  * each word-like "token" is categorised as one of {Ident,Other,Cmd}
--  * #define's are parsed and returned out-of-band using the Cmd variant
--  * All whitespace is preserved intact as tokens.
--  * C-comments are converted to white-space.
--  * Parens and commas are tokens in their own right.
--  * The cpp ## catenation operator is stripped.
--  * Any cpp line continuations are respected.
-- No errors can be raised.
-- The inverse of tokenise is (concatMap deWordStyle).
tokenise :: String -> [WordStyle]
--tokenise :: String -> [Either String HashDefine]
tokenise = haskell Any []
  where
    -- rules to lex Haskell
    haskell :: SubMode -> String -> String -> [WordStyle]
    haskell _   acc []                 = emit acc $ []
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

    -- rules to lex Cpp
    cpp :: SubMode -> String -> [String] -> String -> [WordStyle]
    cpp _   w l []                  = []
    cpp Any w l ('/':'*':xs)        = cpp (NestComment 0) "  " (w*/*l) xs
    cpp Any w l ('/':'/':xs)        = cpp LineComment "  " (w*/*l) xs
    cpp Any w l ('\\':'\n':xs)      = cpp Any [] ("\n":w*/*l) xs
    cpp Any w l xs@('\n':_)         = Cmd (parseHashDefine (reverse (w*/*l))):
                                      haskell Any [] xs
    cpp Any w l ('"':xs)            = cpp (String '"') ['"'] (w*/*l) xs
    cpp Any w l ('\'':xs)           = cpp (String '\'') "'"  (w*/*l) xs
    cpp Any [] l (x:xs) | ident0 x  = cpp (Pred ident1 Ident) [x] l xs
    cpp Any w l (x:xs)  | single x  = cpp Any [] ([x]:w*/*l) xs
                        | space x   = cpp (Pred space Other) [x] (w*/*l) xs
                        | symbol x  = cpp (Pred symbol  Other) [x] (w*/*l) xs
                        | otherwise = cpp Any (x:w) l xs

    cpp pred@(Pred p _) w l (x:xs)
                        | p x       = cpp pred (x:w) l xs
                        | otherwise = cpp Any [] (w*/*l) (x:xs)
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
                                    = cpp Any [] (("  "++w)*/*l) xs
    cpp (NestComment n) w l (x:xs)  = cpp (NestComment n) (' ':w) l xs

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
