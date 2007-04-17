-----------------------------------------------------------------------------
-- |
-- Module      :  Options
-- Copyright   :  2006 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- This module deals with Cpphs options and parsing them
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.Options(CpphsOption(..), parseOption) where

import Maybe

data CpphsOption
    = CpphsNoMacro
    | CpphsNoLine
    | CpphsPragma
    | CpphsText
    | CpphsStrip
    | CpphsAnsi
    | CpphsLayout
    | CpphsUnlit
    | CpphsSuppressWarnings
    | CpphsMacro (String,String)
    | CpphsPath String
      deriving (Eq, Show)
    
    
flags :: [(String, CpphsOption)]
flags = [ ("--nomacro", CpphsNoMacro)
        , ("--noline",  CpphsNoLine)
        , ("--pragma",  CpphsPragma)
        , ("--text",    CpphsText)
        , ("--strip",   CpphsStrip)
        , ("--hashes",  CpphsAnsi)
        , ("--layout",  CpphsLayout)
        , ("--unlit",   CpphsUnlit)
        , ("--nowarn",  CpphsSuppressWarnings)
        ]


parseOption :: String -> Maybe CpphsOption
parseOption x | isJust a = Just $ fromJust a
    where a = lookup x flags

parseOption ('-':'D':xs) = Just $ CpphsMacro (s, if null d then "1" else tail d)
    where (s,d) = break (=='=') xs
    
parseOption ('-':'I':xs) = Just $ CpphsPath $ trail "/\\" xs

parseOption _ = Nothing

trail :: (Eq a) => [a] -> [a] -> [a]
trail xs = reverse . dropWhile (`elem`xs) . reverse

