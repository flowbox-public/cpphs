-- The purpose of this module is to lex a source file (language
-- unspecified) into tokens such that cpp can recognise a replaceable
-- symbol or macro-use, and do the right thing.
module Tokenise
  ( linesCpp
  ) where

-- | A Mode value describes whether to tokenise a la Haskell, or a la Cpp.
--   The main difference is that in Cpp mode we should recognise line
--   continuation characters.
data Mode = Haskell | Cpp

{-
-- tokenise is, broadly-speaking, Prelude.words, except that:
--  * all whitespace is preserved intact as tokens
--  * C-comments are converted to white-space
--  * The cpp ## catenation operator is stripped
-- No errors can be raised.  The inverse of tokenise is concat.
tokenise :: String -> [String]
tokenise = tok Haskell
  where
    tok Haskell ('\n':'#':xs)    = "\n": "#": tok Cpp xs
    tok Cpp     ('\\':'\n':xs)   = tok Cpp xs
    tok Cpp     ('#':'#':xs)     = tok Cpp xs
    tok _       ('\n':xs)        = "\n": tok Haskell xs
    tok mode    (x:xs) | space x = accumulate space [x] xs (tok mode)
    tok mode    (x:xs) | sym x   = accumulate sym [x] xs (tok mode)
    tok _       []               = []

    accumulate pred acc (x:xs) k | pred x = accumulate pred (x:acc) xs k
    accumulate pred acc xs     k          = reverse acc: k xs
-}


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

