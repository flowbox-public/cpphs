-----------------------------------------------------------------------------
-- |
-- Module      :  MacroPass
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Perform a cpp.second-pass, accumulating #define's and #undef's,
-- whilst doing symbol replacement and macro expansion.
-----------------------------------------------------------------------------

module MacroPass
  ( macroPass
  , preDefine
  ) where

import HashDefine (HashDefine(..), expandMacro)
import Tokenise   (tokenise, WordStyle(..), parseMacroCall)
import SymTab     (SymTab, lookupST, insertST, emptyST)

macroPass :: [String]		-- ^ Pre-defined symbols
          -> Bool		-- ^ Strip C-comments?
          -> Bool		-- ^ Accept # and ## operators?
          -> String		-- ^ The input file content
          -> String		-- ^ The file after processing
macroPass syms strip hashes =
    concat . macroProcess (preDefine hashes syms) . tokenise strip hashes


-- | Command-line definitions via -D are parsed here
preDefine :: Bool -> [String] -> SymTab HashDefine
preDefine hashes defines =
    foldr (insertST.defval) emptyST defines
  where
    defval sym =
        let (s,d) = break (=='=') sym
            (Cmd (Just hd):_) = tokenise True hashes
                                         ("\n#define "++s++" "++rmEq d++"\n")
            rmEq [] = []
            rmEq (x:xs) = xs
        in (name hd, hd)


-- Trundle through the document, one word at a time, using the WordStyle
-- classification introduced by 'tokenise' to decide whether to expand a
-- word or macro.  Encountering a #define or #undef causes that symbol to
-- be overwritten in the symbol table.  Any other remaining cpp directives
-- are discarded and replaced with blanks, except for #line markers.
-- All valid identifiers are checked for the presence of a definition
-- of that name in the symbol table, and if so, expanded appropriately.
macroProcess :: SymTab HashDefine -> [WordStyle] -> [String]
macroProcess st        []                     = []
macroProcess st (Other x: ws)                 = x:    macroProcess st ws
macroProcess st (Cmd Nothing: ws)             = "\n": macroProcess st ws
macroProcess st (Cmd (Just (LineDrop x)): ws) = "\n":x: macroProcess st ws
macroProcess st (Cmd (Just hd): ws)           =
        let n = 1 + linebreaks hd in
        replicate n "\n" ++ macroProcess (insertST (name hd, hd) st) ws
macroProcess st (Ident x: ws) =
        case lookupST x st of
            Nothing -> x: macroProcess st ws
            Just hd ->
                case hd of
                    SymbolReplacement _ r _ ->
                        -- one-level expansion only:
                        -- r: macroProcess st ws
                        -- multi-level expansion:
                        macroProcess st (tokenise True False r ++ ws)
                    MacroExpansion _ _ _ _  ->
                        case parseMacroCall ws of
                            Nothing -> x: macroProcess st ws
                            Just (args,ws') ->
                                if length args /= length (arguments hd) then
                                     x: macroProcess st ws
                                else -- one-level expansion only:
                                     -- expandMacro hd args: macroProcess st ws'
                                     -- multi-level expansion:
                                     macroProcess st (tokenise True False
                                                        (expandMacro hd args)
                                                     ++ws')

