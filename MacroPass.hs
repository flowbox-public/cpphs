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
import Position   (Posn, newfile, filename, lineno)
import System.IO.Unsafe (unsafePerformIO)
import Time       (getClockTime, toCalendarTime, formatCalendarTime)
import Locale     (defaultTimeLocale)

noPos :: Posn
noPos = newfile "preDefined"

macroPass :: [String]		-- ^ Pre-defined symbols
          -> Bool		-- ^ Strip C-comments?
          -> Bool		-- ^ Accept # and ## operators?
          -> Bool		-- ^ Retain layout in macros?
          -> [(Posn,String)]	-- ^ The input file content
          -> String		-- ^ The file after processing
macroPass syms strip hashes layout =
    tail		-- to remove extra "\n" inserted below
    . concat
    . macroProcess layout (preDefine hashes syms)
    . tokenise strip hashes
    . ((noPos,""):)	-- ensure recognition of "\n#" at start of file


-- | Command-line definitions via -D are parsed here
preDefine :: Bool -> [String] -> SymTab HashDefine
preDefine hashes defines =
    foldr (insertST.defval) emptyST defines
  where
    defval sym =
        let (s,d) = break (=='=') sym
            (Cmd (Just hd):_) = tokenise True hashes
                                   [(noPos,"\n#define "++s++" "++rmEq d++"\n")]
            rmEq [] = []
            rmEq (_:xs) = xs
        in (name hd, hd)


-- Trundle through the document, one word at a time, using the WordStyle
-- classification introduced by 'tokenise' to decide whether to expand a
-- word or macro.  Encountering a #define or #undef causes that symbol to
-- be overwritten in the symbol table.  Any other remaining cpp directives
-- are discarded and replaced with blanks, except for #line markers.
-- All valid identifiers are checked for the presence of a definition
-- of that name in the symbol table, and if so, expanded appropriately.
macroProcess :: Bool -> SymTab HashDefine -> [WordStyle] -> [String]
macroProcess _  _         []                     = []
macroProcess ly st (Other x: ws)                 = x:    macroProcess ly st ws
macroProcess ly st (Cmd Nothing: ws)             = "\n": macroProcess ly st ws
macroProcess ly st (Cmd (Just (LineDrop x)): ws) = "\n":x:macroProcess ly st ws
macroProcess layout st (Cmd (Just hd): ws)       =
    let n = 1 + linebreaks hd in
    replicate n "\n" ++ macroProcess layout (insertST (name hd, hd) st) ws
macroProcess layout st (Ident p x: ws) =
    case x of
      "__FILE__" -> filename p:      macroProcess layout st ws
      "__LINE__" -> show (lineno p): macroProcess layout st ws
      "__DATE__" -> formatCalendarTime defaultTimeLocale "%d %b %Y"
                        (unsafePerformIO (getClockTime>>=toCalendarTime)):
                                     macroProcess layout st ws
      "__TIME__" -> formatCalendarTime defaultTimeLocale "%H:%M:%S"
                        (unsafePerformIO (getClockTime>>=toCalendarTime)):
                                     macroProcess layout st ws
      _ ->
        case lookupST x st of
            Nothing -> x: macroProcess layout st ws
            Just hd ->
                case hd of
                    SymbolReplacement _ r _ ->
                        -- one-level expansion only:
                        -- r: macroProcess layout st ws
                        -- multi-level expansion:
                        let r' = if layout then r else filter (/='\n') r in
                        macroProcess layout st (tokenise True False [(p,r')]
                                               ++ ws)
                    MacroExpansion _ _ _ _  ->
                        case parseMacroCall ws of
                            Nothing -> x: macroProcess layout st ws
                            Just (args,ws') ->
                                if length args /= length (arguments hd) then
                                     x: macroProcess layout st ws
                                else -- one-level expansion only:
                                     -- expandMacro hd args layout:
                                     --         macroProcess layout st ws'
                                     -- multi-level expansion:
                                     macroProcess layout st
                                              (tokenise True False
                                                [(p,expandMacro hd args layout)]
                                              ++ ws')

