{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Printf.Parse where

import Control.Applicative hiding (many)
import Data.Char
import Data.String
import Data.Foldable
import Data.Proxy
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char

import Language.Printf.FormatString

type MonadParsecChar e s m = (MonadParsec e s m, Token s ~ Char)

type MonadParsecString e s m = (MonadParsecChar e s m, IsString (Tokens s))

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
  , ('+',  PositivePlus)
  , (' ',  PositiveSpace)
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
  , ('s', String)
  , ('p', Pointer)
  , ('%', Percent)
    -- Omitted: %n
  ---- Extensions ----
  -- Strings
  , ('S', SpecificString)
  -- Haskell
  , ('?', Showable)
  -- User-extensible formatting
  , ('@', Formattable) -- Could split into one for a -> buffer and one for (flags, length, a) -> buffer
  , ('v', Function Simple) -- Lowercase letters are reserved by the C standard; could rework
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
  Byte                        <$  string "hh"                      <|>
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
