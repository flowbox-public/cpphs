-----------------------------------------------------------------------------
-- |
-- Module      :  ReadFirst
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Read the first file that matches in a list of search paths.
-----------------------------------------------------------------------------

module ReadFirst
  ( readFirst
  ) where

import IO        (readFile)
import Directory (doesFileExist)
import List      (intersperse)
import Position  (Posn)
import SymTab    (SymTab,lookupST)

-- | Attempt to read the given file from any location within the search path.
--   The first location found is returned, together with the file content.
--   (The current directory is always searched first.)
readFirst :: String		-- ^ filename
	-> Posn			-- ^ inclusion point
	-> [String]		-- ^ search path
	-> SymTab String	-- ^ #defined symbols
	-> IO ( FilePath	-- ^ discovered file name, inc path
              , String		-- ^ file contents
              )

readFirst name demand path syms =
    try (".":path)	-- always search current directory first
  where
    realname = real name syms
    try [] = error ("Can't find file \""++realname++"\" in directories\n\t"
                    ++concat (intersperse "\n\t" (".":path))
                    ++"\n  Asked for by: "++show demand)
    try (p:ps) = do
        let file = p++'/':realname
        ok <- doesFileExist file
        if not ok then try ps
          else do content <- readFile file
                  return (file,content)

real name syms = case name of
                   ('"':ns) -> init ns
                   ('<':ns) -> init ns
                   _        -> case lookupST name syms of
                                 Nothing -> name
                                 Just f  -> real f syms

