-----------------------------------------------------------------------------
-- |
-- Module      :  HashDefine
-- Copyright   :  2004 Malcolm Wallace
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
	= SymbolReplacement
		{ name		:: String
		, replacement	:: String }
	| MacroExpansion
		{ name		:: String
		, arguments	:: [String]
		, expansion	:: [(ArgOrText,String)]
		}
    deriving (Eq,Show)

data ArgOrText = Arg | Text deriving (Eq,Show)

-- expand an instance of a macro
-- precondition: got a match on the macro name.
expandMacro :: HashDefine -> [String] -> String
expandMacro macro parameters =
    let env = zip (arguments macro) parameters
        replace (Arg,s)  = maybe (error "formal param") id (lookup s env)
        replace (Text,s) = s
    in
    concatMap replace (expansion macro)

-- parse a #define, or #undef, ignoring other # directives
parseHashDefine :: [String] -> Maybe HashDefine
parseHashDefine = command . skip
  where
    skip xss@(x:xs) | all isSpace x = skip xs
                    | otherwise     = xss
    skip    []      = []
    command ("define":xs) = Just ((define . skip) xs)
    command ("undef":xs)  = Just ((undef  . skip) xs)
    command _             = Nothing
    undef  (sym:_)   = SymbolReplacement { name=sym, replacement=sym }
    define (sym:xs)  = case skip xs of
                           ("(":ys) -> (macro sym [] . skip) ys
                           ys       -> SymbolReplacement
                                           { name=sym, replacement=concat ys }
    macro sym args (",":xs) = (macro sym args . skip) xs
    macro sym args (")":xs) = MacroExpansion
                                    { name =sym , arguments = reverse args
                                    , expansion = classify args (skip xs) }
    macro sym args (var:xs) = (macro sym (var:args) . skip) xs
    macro sym args []       = error ("incomplete macro definition:\n"
                                    ++"  #define "++sym++"("
                                    ++concat (intersperse "," args))
    classify args ("##":xs) = classify args xs
    classify args (word:xs) | word `elem` args = (Arg,word): classify args xs
                            | otherwise        = (Text,word): classify args xs
    classify args []        = []


-- test data
simple, complex :: HashDefine
simple  = SymbolReplacement { name = "CALLCONV", replacement = "ccall" }
complex = MacroExpansion
	{ name = "EXTENSION_ENTRY"
	, arguments = ["_msg","_entry","_ty"]
	, expansion =
		[(Text,"foreign import CALLCONV unsafe \"dynamic\" dyn_")
		,(Arg,"_entry")
		,(Text," :: Graphics.Rendering.OpenGL.GL.Extensions.Invoker (")
		,(Arg,"_ty")
		,(Text,") ; ")
		,(Arg,"_entry")
		,(Text," :: (")
		,(Arg,"_ty")
		,(Text,") ; ")
		,(Arg,"_entry")
		,(Text," = dyn_")
		,(Arg,"_entry")
		,(Text," ptr_")
		,(Arg,"_entry")
		,(Text," ; ptr_")
		,(Arg,"_entry")
		,(Text," :: FunPtr a ; ptr_")
		,(Arg,"_entry")
		,(Text," = unsafePerformIO (Graphics.Rendering.OpenGL.GL.Extensions.getProcAddress (")
		,(Arg,"_msg")
		,(Text,") (\"")
		,(Arg,"_entry")
		,(Text,"\")) ; {-# NOINLINE ptr_")
		,(Arg,"_entry")
		,(Text," #-}")
		]
	}
