module MacroPass
  ( macroPass
  , preDefine
  ) where

import HashDefine (HashDefine(..), expandMacro)
import Tokenise   (tokenise, WordStyle(..), parseMacroCall)
import SymTab     (SymTab, lookupST, insertST, emptyST)

macroPass :: [String]		-- ^ Pre-defined symbols
          -> String		-- ^ The input file content
          -> String		-- ^ The file after processing
macroPass syms =
    concat . macroProcess (preDefine syms) . tokenise


-- | Command-line definitions via -D are parsed here
preDefine :: [String] -> SymTab HashDefine
preDefine defines =
    foldr (insertST.defval) emptyST defines
  where
    defval sym =
        let (s,d) = break (=='=') sym
            (Cmd (Just hd):_) = tokenise ("\n#define "++s++" "++tail d++"\n")
        in (name hd, hd)


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
                    SymbolReplacement _ r _ -> r: macroProcess st ws
                    MacroExpansion _ _ _ _  ->
                        case parseMacroCall ws of
                            Nothing -> x: macroProcess st ws
                            Just (args,ws') ->
                                if length args /= length (arguments hd) then
                                     x: macroProcess st ws
                                else -- one-level expansion only:
                                     -- expandMacro hd args: macroProcess st ws'
                                     -- multi-level expansion:
                                     macroProcess st (tokenise
                                                        (expandMacro hd args)
                                                     ++ws')

