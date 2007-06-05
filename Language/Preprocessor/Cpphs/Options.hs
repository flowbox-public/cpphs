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

module Language.Preprocessor.Cpphs.Options
  ( CpphsOptions(..)
  , BoolOptions(..)
  , parseOptions
  , defaultCpphsOptions
  , defaultBoolOptions
  ) where

import Maybe

-- | Cpphs options structure.
data CpphsOptions = CpphsOptions 
    { infiles	:: [FilePath]
    , outfiles	:: [FilePath]
    , defines	:: [(String,String)]
    , includes	:: [String]
    , boolopts	:: BoolOptions
    }

-- | Default options.
defaultCpphsOptions :: CpphsOptions
defaultCpphsOptions = CpphsOptions { infiles = [], outfiles = []
                                   , defines = [], includes = []
                                   , boolopts = defaultBoolOptions }

-- | Options representable as Booleans.
data BoolOptions = BoolOptions
    { macros	:: Bool  -- ^ Leave \#define and \#undef in output of ifdef?
    , locations	:: Bool	 -- ^ Place \#line droppings in output?
    , pragma	:: Bool  -- ^ Keep \#pragma in final output?
    , strip	:: Bool  -- ^ Remove C comments everywhere?
    , lang	:: Bool  -- ^ Lex input as Haskell code?
    , ansi	:: Bool  -- ^ Permit stringise # and catenate ## operators?
    , layout	:: Bool  -- ^ Retain newlines in macro expansions?
    , literate	:: Bool  -- ^ Remove literate markup?
    , warnings	:: Bool  -- ^ Issue warnings?
    }

-- | Default settings of boolean options.
defaultBoolOptions :: BoolOptions
defaultBoolOptions = BoolOptions { macros   = True,   locations = True
                                 , pragma   = False,  strip     = False
                                 , lang     = True,   ansi      = False
                                 , layout   = False,  literate  = False
                                 , warnings = True }

-- | Raw command-line options.  This is an internal intermediate data
--   structure, used during option parsing only.
data RawOption
    = NoMacro
    | NoLine
    | Pragma
    | Text
    | Strip
    | Ansi
    | Layout
    | Unlit
    | SuppressWarnings
    | Macro (String,String)
    | Path String
      deriving (Eq, Show)

flags :: [(String, RawOption)]
flags = [ ("--nomacro", NoMacro)
        , ("--noline",  NoLine)
        , ("--pragma",  Pragma)
        , ("--text",    Text)
        , ("--strip",   Strip)
        , ("--hashes",  Ansi)
        , ("--layout",  Layout)
        , ("--unlit",   Unlit)
        , ("--nowarn",  SuppressWarnings)
        ]

-- | Parse a single raw command-line option.  Parse failure is indicated by
--   result Nothing.
rawOption :: String -> Maybe RawOption
rawOption x | isJust a = a
    where a = lookup x flags
rawOption ('-':'D':xs) = Just $ Macro (s, if null d then "1" else tail d)
    where (s,d) = break (=='=') xs
rawOption ('-':'I':xs) = Just $ Path $ trailing "/\\" xs
rawOption _ = Nothing

trailing :: (Eq a) => [a] -> [a] -> [a]
trailing xs = reverse . dropWhile (`elem`xs) . reverse

-- | Convert a list of RawOption to a BoolOptions structure.
boolOpts :: [RawOption] -> BoolOptions
boolOpts opts =
  BoolOptions
    { macros	= not (NoMacro `elem` opts)
    , locations	= not (NoLine  `elem` opts)
    , pragma	=      Pragma  `elem` opts
    , strip	=      Strip   `elem` opts
    , lang      = not (Text    `elem` opts)
    , ansi	=      Ansi    `elem` opts
    , layout	=      Layout  `elem` opts
    , literate	=      Unlit   `elem` opts
    , warnings	= not (SuppressWarnings `elem` opts)
    }

-- | Parse all command-line options.
parseOptions :: [String] -> Either String CpphsOptions
parseOptions xs = f ([], [], []) xs
  where
    f (opts, ins, outs) (('-':'O':x):xs) = f (opts, ins, x:outs) xs
    f (opts, ins, outs) (x@('-':_):xs) = case rawOption x of
                                           Nothing -> Left x
                                           Just a  -> f (a:opts, ins, outs) xs
    f (opts, ins, outs) (x:xs) = f (opts, normalise x:ins, outs) xs
    f (opts, ins, outs) []     =
        Right CpphsOptions { infiles  = reverse ins
                           , outfiles = reverse outs
                           , defines  = [ x | Macro x <- reverse opts ]
                           , includes = [ x | Path x  <- reverse opts ]
                           , boolopts = boolOpts opts
                           }
    normalise ('/':'/':filepath) = normalise ('/':filepath)
    normalise (x:filepath)       = x:normalise filepath
    normalise []                 = []
