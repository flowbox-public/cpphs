-----------------------------------------------------------------------------
-- |
-- Module      :  Position
-- Copyright   :  2000-2004 Malcolm Wallace
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Simple file position information, with recursive inclusion points.
-----------------------------------------------------------------------------

module Position
  ( Posn(..)
  , newfile
  , addcol, newline, tab, newlines, hashline
  , cppline
  ) where

-- | Source positions contain a filename, line, column, and an
--   inclusion point, which is itself another source position,
--   recursively.
data Posn = Pn String !Int !Int (Maybe Posn)
        deriving (Eq)

instance Show Posn where
      showsPrec p (Pn f l c i) = showString f .
                                 showString "  at line " . shows l .
                                 showString " col " . shows c .
                                 ( case i of
                                    Nothing -> id
                                    Just p  -> showString "\n    used by  " .
                                               shows p )

-- | Constructor
newfile :: String -> Posn
newfile name = Pn name 1 1 Nothing

-- | Updates
addcol :: Int -> Posn -> Posn
addcol n (Pn f r c i) = Pn f r (c+n) i

newline, tab :: Posn -> Posn
newline (Pn f r c i) = Pn f (r+1) 1 i
tab     (Pn f r c i) = Pn f r (((c`div`8)+1)*8) i

newlines :: Int -> Posn -> Posn
newlines n (Pn f r c i) = Pn f (r+n) 1 i

hashline :: Int -> Maybe String -> Posn -> Posn
hashline r Nothing  (Pn f _ c i) = Pn f r c i
hashline r (Just ('"':f)) (Pn _ _ c i) = Pn (init f) r c i
hashline r (Just f)       (Pn _ _ c i) = Pn f r c i

{-
-- | Projections
lineno   :: Posn -> Int
filename :: Posn -> String

lineno   (Pn _ r _ _) = r
filename (Pn f _ _ _) = f
-}

-- | cpp-style printing
cppline :: Posn -> String
cppline (Pn f r c i) = "#line "++show r++" "++show f
