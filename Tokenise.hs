-- The purpose of this module is to lex a source file (language
-- unspecified) into tokens such that cpp can recognise a replaceable
-- symbol or macro-use, and do the right thing.
module Tokenise
  ( linesCpp
  , tokenise
  ) where

import Char

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
data SubMode = Any | Pred (Char->Bool)
             | String Char | LineComment | NestComment Int
             | Hash

tokenise :: String -> [Either String ()]
--tokenise :: String -> [Either String HashDefine]
tokenise = tok Haskell Any []
  where
    tok Haskell _   acc []                 = emit acc $ []
    tok Haskell Any acc ('\n':'#':xs)      = emit acc $ Left "\n":
                                             tok Cpp Hash [] xs
						-- warning: non-maximal munch
    tok Haskell Any acc ('-':'-':xs)       = emit acc $
                                             tok Haskell LineComment "--" xs
    tok Haskell Any acc ('{':'-':xs)       = emit acc $
                                             tok Haskell (NestComment 0) "-{" xs
    tok Haskell Any acc ('"':xs)           = emit acc $
                                             tok Haskell (String '"') ['"'] xs
    tok Haskell Any acc ('\'':xs)          = emit acc $
                                             tok Haskell (String '\'') "'" xs
    tok Haskell Any acc (x:xs) | single x  = emit acc $ Left [x]:
                                             tok Haskell Any [] xs
    tok Haskell Any acc (x:xs) | isSpace x = emit acc $
                                             tok Haskell (Pred isSpace) [x] xs
    tok Haskell Any acc (x:xs) | symbol x  = emit acc $
                                             tok Haskell (Pred symbol) [x] xs
    tok Haskell Any []  (x:xs) | ident0 x  = tok Haskell (Pred ident1) [x] xs
    tok Haskell Any acc (x:xs)             = tok Haskell Any (x:acc) xs

    tok Haskell pred@(Pred p) acc (x:xs)
                               | p x       = tok Haskell pred (x:acc) xs
                               | otherwise = emit acc $
                                             tok Haskell Any [] (x:xs)
    tok Haskell (String c) acc ('\\':x:xs)
                               | x=='\\'   = tok Haskell (String c)
                                                             ('\\':'\\':acc) xs
                               | x==c      = tok Haskell (String c)
                                                                (c:'\\':acc) xs
    tok Haskell (String c) acc (x:xs)
                               | x==c      = emit (c:acc) $
                                             tok Haskell Any [] xs
                               | otherwise = tok Haskell (String c) (x:acc) xs
    tok Haskell LineComment acc ('\n':xs)  = emit ('\n':acc) $
                                             tok Haskell Any [] xs
    tok Haskell LineComment acc (x:xs)     = tok Haskell LineComment (x:acc) xs
    tok Haskell (NestComment n) acc ('{':'-':xs)
                                           = tok Haskell (NestComment (n+1))
                                                                 ("-{"++acc) xs
    tok Haskell (NestComment 0) acc ('-':'}':xs)
                                           = emit ("}-"++acc) $
                                             tok Haskell Any [] xs
    tok Haskell (NestComment n) acc ('-':'}':xs)
                                           = tok Haskell (NestComment (n-1))
                                                                 ("}-"++acc) xs
    tok Haskell (NestComment n) acc (x:xs) = tok Haskell (NestComment n)
                                                                     (x:acc) xs

    ident0 x = isAlpha x    || x `elem` "_`"
    ident1 x = isAlphaNum x || x `elem` "'_`"
    symbol x = x `elem` ":!#$%&*+./<=>?@\\^|-~"
    single x = x `elem` "(),[];{}"
    emit ""  = id
    emit xs  = (Left (reverse xs):)

