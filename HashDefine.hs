-----------------------------------------------------------------------------
-- |
-- Module      :  HashDefine
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- What structures are declared in a #define.
-----------------------------------------------------------------------------
 
module HashDefine
  ( HashDefine(..)
  , expandMacro
  , parseHashDefine
  ) where

import Char (isSpace)
import List (intersperse)

data HashDefine
	= LineDrop
		{ name :: String }
	| SymbolReplacement
		{ name		:: String
		, replacement	:: String
		, linebreaks    :: Int
		}
	| MacroExpansion
		{ name		:: String
		, arguments	:: [String]
		, expansion	:: [(ArgOrText,String)]
		, linebreaks    :: Int
		}
    deriving (Eq,Show)

data ArgOrText = Arg | Text | Str deriving (Eq,Show)

-- expand an instance of a macro
-- precondition: got a match on the macro name.
expandMacro :: HashDefine -> [String] -> String
expandMacro macro parameters =
    let env = zip (arguments macro) parameters
        replace (Arg,s)  = maybe (error "formal param") id (lookup s env)
        replace (Str,s)  = maybe (error "formal param") str (lookup s env)
        replace (Text,s) = s
        str s = '"':s++"\""
    in
    concatMap replace (expansion macro)

-- | parse a #define, or #undef, ignoring other # directives
parseHashDefine :: Bool -> [String] -> Maybe HashDefine
parseHashDefine ansi def = (command . skip) def
  where
    skip xss@(x:xs) | all isSpace x = skip xs
                    | otherwise     = xss
    skip    []      = []
    command ("line":xs)   = Just (LineDrop ("#line"++concat xs))
    command ("define":xs) = Just (((define . skip) xs) { linebreaks=count def })
    command ("undef":xs)  = Just (((undef  . skip) xs) { linebreaks=count def })
    command _             = Nothing
    undef  (sym:_)   = SymbolReplacement { name=sym, replacement=sym }
    define (sym:xs)  = case skip xs of
                           ("(":ys) -> (macro sym [] . skip) ys
                           ys       -> SymbolReplacement
                                           { name=sym, replacement=chop ys }
    macro sym args (",":xs) = (macro sym args . skip) xs
    macro sym args (")":xs) = MacroExpansion
                                    { name =sym , arguments = reverse args
                                    , expansion = classify args (skip xs) }
    macro sym args (var:xs) = (macro sym (var:args) . skip) xs
    macro sym args []       = error ("incomplete macro definition:\n"
                                    ++"  #define "++sym++"("
                                    ++concat (intersperse "," args))
    classify args ("#":x:xs)| ansi &&
                              x `elem` args    = (Str,x): classify args xs
    classify args ("##":xs) | ansi             = classify args xs
    classify args (word:xs) | word `elem` args = (Arg,word): classify args xs
                            | otherwise        = (Text,word): classify args xs
    classify args []        = []
    count = length . filter (=='\n') . concat
    chop  = concat . reverse . dropWhile (all isSpace) . reverse

