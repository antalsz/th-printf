{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Printf.Parse where

import Control.Applicative hiding (many)
import Data.Char hiding (GeneralCategory(..))
import Data.String
import Data.Foldable
import Data.Proxy
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char

type MonadParsecChar e s m = (MonadParsec e s m, Token s ~ Char)

type MonadParsecString e s m = (MonadParsecChar e s m, IsString (Tokens s))

data PositiveSign = Plus | Space
                  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Flag = LeftJustify
          | Signed PositiveSign
          | Alternative
          | ZeroPadding
          | ThousandsSeparators -- POSIX
          deriving (Eq, Ord, Show, Read)

data Padding = Zeros | Spaces
             deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Argument = InOrder
              | Positional Word -- POSIX
              deriving (Eq, Ord, Show, Read)

data IntParameter = Fixed Word
                  | Argument Argument
                  deriving (Eq, Ord, Show, Read)

newtype Width = Width IntParameter
  deriving (Eq, Ord, Show, Read)

newtype Precision = Precision IntParameter
  deriving (Eq, Ord, Show, Read)

data IntWidthPrecision = Exact | FastestAtLeast
                       deriving (Eq, Ord, Show, Read, Enum, Bounded)

data DecimalWidth = D32 | D64 | D128
                  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Length = Char
            | Short
            | Long
            | LongLong
            | IntMax
            | Size
            | PtrDiff
            | IntWithWidth IntWidthPrecision Word
            | LongDouble
            | DecimalFloat DecimalWidth
            deriving (Eq, Ord, Show, Read)

data Case = Lowercase | Uppercase
          deriving (Eq, Ord, Show, Read, Enum, Bounded)

data IntegerStyle = SignedDecimal
                  | Unsigned IntegerBase
                  deriving (Eq, Ord, Show, Read)

data IntegerBase = Binary Case -- C24
                 | Decimal
                 | Octal
                 | Hexadecimal Case
                 deriving (Eq, Ord, Show, Read)

data FloatingStyle = DecimalDigits
                   | DecimalExponent
                   | DecimalShortest
                   | HexadecimalExponent
                   deriving (Eq, Ord, Show, Read, Enum, Bounded)

data StringType = Generic
                | HaskellString -- HS extension
                | StrictText -- HS extension
                | LazyText -- HS extension
                deriving (Eq, Ord, Show, Read)

data FunctionType = Simple | Full
                  -- HS extension
                  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Specifier = Integer IntegerStyle
               | FloatingPoint FloatingStyle Case
               | Character
               | String StringType
               | Pointer
               -- No %n, which would be StoreOutputLength
               | Formattable -- HS extension
               | Showable -- HS extension
               | Function FunctionType -- HS extension
               | Buffer -- HS extension
               | Percent
               deriving (Eq, Ord, Show, Read)

data FormatSpecifier = FormatSpecifier
  { argument       :: Maybe Word
  , flags          :: [Flag]
  , width          :: Maybe Width
  , precision      :: Maybe Precision
  , lengthModifier :: Maybe Length
  , specifier      :: Specifier
  }
  deriving (Eq, Ord, Show, Read)

data FormatStringChunk str = LiteralChunk str
                           | FormatSpecifierChunk FormatSpecifier
                           deriving (Eq, Ord, Show, Read)

instance IsString str => IsString (FormatStringChunk str) where
  fromString = LiteralChunk . fromString

newtype FormatString str = FormatString [FormatStringChunk str]
  deriving (Eq, Ord, Show, Read, Semigroup, Monoid)

singleCharacter :: MonadParsecChar e s m => String -> [(Char, a)] -> m a
singleCharacter what pairs
  | length (S.fromList chars) == length chars =
      asum [spec <$ char c | (c, spec) <- pairs] <?> (what ++ " (one of " ++ chars ++ ")")
  | otherwise =
      error $ "[INTERNAL ERROR] When defining " ++ what ++ ", found duplicate characters"
  where
    chars = map fst pairs

-- defined like isDigit
isPositiveDigit :: Char -> Bool
isPositiveDigit c = (fromIntegral @_ @Word (ord c - ord '1')) < 9

-- Modeled after 'decimal'
positiveInteger :: forall e s m a. (MonadParsecChar e s m, Num a) => m a
positiveInteger = label "positive integer" do
  digit  <- satisfy isPositiveDigit <?> "nonzero digit"
  digits <- takeWhileP (Just "digit") isDigit
  pure . foldl' (\n d -> 10*n + fromIntegral (digitToInt d)) 0 $ digit : chunkToTokens (Proxy @s) digits

positionalArgument :: (MonadParsecChar e s m, Num a) => m a
positionalArgument = positiveInteger <* char '$' <?> "positional argument"

flag :: MonadParsecChar e s m => m Flag
flag = singleCharacter "flag"
  [
  ---- C standard ----
    ('-',  LeftJustify)
  , ('+',  Signed Plus)
  , (' ',  Signed Space)
  , ('#',  Alternative)
  , ('0',  ZeroPadding)
  ---- POSIX ----
  , ('\'', ThousandsSeparators)
  ]

parseSpecifier :: MonadParsecChar e s m => m Specifier
parseSpecifier = singleCharacter "specifier"
  [
  ---- C(++) standard ----
    ('d', Integer SignedDecimal)
  , ('i', Integer SignedDecimal)
  , ('b', Integer . Unsigned $ Binary Lowercase)
  , ('B', Integer . Unsigned $ Binary Uppercase)
  , ('u', Integer $ Unsigned Decimal)
  , ('o', Integer $ Unsigned Octal)
  , ('x', Integer . Unsigned $ Hexadecimal Lowercase)
  , ('X', Integer . Unsigned $ Hexadecimal Uppercase)
  , ('f', FloatingPoint DecimalDigits Lowercase)
  , ('F', FloatingPoint DecimalDigits Uppercase)
  , ('e', FloatingPoint DecimalExponent Lowercase)
  , ('E', FloatingPoint DecimalExponent Uppercase)
  , ('g', FloatingPoint DecimalShortest Lowercase)
  , ('G', FloatingPoint DecimalShortest Uppercase)
  , ('a', FloatingPoint HexadecimalExponent Lowercase)
  , ('A', FloatingPoint HexadecimalExponent Uppercase)
  , ('c', Character)
  , ('s', String Generic)
  , ('p', Pointer)
  , ('%', Percent)
    -- Omitted: %n
  ---- Extensions ----
  -- Strings
  , ('S', String HaskellString)
  -- Haskell
  , ('?', Showable)
  -- User-extensible formatting
  , ('@', Formattable)
  , ('v', Function Simple)
  , ('V', Function Full)
  -- Buffer
  , ('|', Buffer) -- could change
  ]

intParameter :: MonadParsecChar e s m => m IntParameter
intParameter =
  Fixed    <$> positiveInteger <|>
  Argument <$  char '*' <*> option InOrder (Positional <$> positionalArgument)

parseWidth :: MonadParsecChar e s m => m Width
parseWidth = Width <$> intParameter <?> "width"

parsePrecision :: MonadParsecChar e s m => m Precision
parsePrecision = Precision <$ char '.' <*> intParameter <?> "precision"

parseLengthModifier :: MonadParsecString e s m => m Length
parseLengthModifier = label "length modifier" $
  Char                        <$  string "hh"                      <|>
  LongLong                    <$  string "ll"                      <|>
  Short                       <$  char 'h'                         <|>
  Long                        <$  char 'l'                         <|>
  IntMax                      <$  char 'j'                         <|>
  Size                        <$  char 'z'                         <|>
  PtrDiff                     <$  char 't'                         <|>
  IntWithWidth FastestAtLeast <$> (string "wf" *> positiveInteger) <|>
  IntWithWidth Exact          <$> (char 'w' *> positiveInteger)    <|>
  LongDouble                  <$  char 'L'                         <|>
  DecimalFloat D32            <$  char 'H'                         <|>
  DecimalFloat D128           <$  string "DD"                      <|>
  DecimalFloat D64            <$  char 'D'

-- %[arg$][flags][width][.precision][length]specifier
formatSpecifier :: MonadParsecString e s m => m FormatSpecifier
formatSpecifier = label "format specifier" do
  _              <- char '%'
  argument       <- optional $ try positionalArgument
  flags          <- many flag
  width          <- optional parseWidth
  precision      <- optional parsePrecision
  lengthModifier <- optional parseLengthModifier
  specifier      <- parseSpecifier
  pure FormatSpecifier{..}

formatStringChunk :: MonadParsecString e s m => m (FormatStringChunk (Tokens s))
formatStringChunk =   LiteralChunk         <$> takeWhile1P (Just "non-% character") (/= '%')
                  <|> FormatSpecifierChunk <$> formatSpecifier

parseFormatString :: MonadParsecString e s m => m (FormatString (Tokens s))
parseFormatString = FormatString <$> many formatStringChunk <?> "format string"

{-
  Might support extension: %hf for float

  Might support extension: %Ld for Integer (but nothing for `Int`?)

  Might support extension: %hS for strict ByteString

  Might support extension: %hhS for lazy ByteString

  NOT SUPPORTING from POSIX (XSI: X/Open System Interfaces), since we're fully
  unicode-aware (and so I can steal %S for my own use):
    %C = %lc
    %S = %ls
-}
