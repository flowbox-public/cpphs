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
  ) where

data HashDefine
	= SymbolReplacement
		{ name		:: String
		, replacement	:: String }
	| MacroExpansion
		{ name		:: String
		, arguments	:: [String]
		, expansion	:: [(ArgOrText,String)]
		}

data ArgOrText = Arg | Text deriving (Eq)

-- expand an instance of a macro
-- precondition: got a match on the macro name.
expandMacro :: HashDefine -> [String] -> String
expandMacro macro parameters =
    let env = zip (arguments macro) parameters
        replace (Arg,s)  = maybe (error "formal param") id (lookup s env)
        replace (Text,s) = s
    in
    concatMap replace (expansion macro)

-- parse a #define


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
