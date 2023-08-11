{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Printf.Parse where

import Control.Applicative hiding (many)
import Control.Monad.Reader
import Data.Char
import Data.String
import Data.Foldable
import Data.Proxy
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char

import Language.Printf.Options
import Language.Printf.FormatString

type Parser e s m = (MonadParsec e s m, Token s ~ Char, IsString (Tokens s), MonadReader Extensions m)

ifAtLeast :: (MonadReader r m, MonadPlus m, Ord r) => r -> m a -> m a
ifAtLeast required m = do
  current <- ask
  if required <= current
  then m
  else empty

singleCharacter :: Parser e s m => String -> [(Char, a, Extensions)] -> m a
singleCharacter what entries
  | length (S.fromList chars) == length chars = do
      current <- ask
      let withEntries f = [f c spec | (c, spec, required) <- entries, required <= current]
      asum (withEntries \c spec -> spec <$ char c) <?> what ++ " (one of " ++ withEntries const ++ ")"
  | otherwise =
      error $ "[INTERNAL ERROR] When defining " ++ what ++ ", found duplicate characters"
  where
    chars = [c | (c, _, _) <- entries]

-- defined like isDigit
isPositiveDigit :: Char -> Bool
isPositiveDigit c = fromIntegral @_ @Word (ord c - ord '1') < 9

-- Modeled after 'decimal'
positiveInteger :: forall e s m a. (Parser e s m, Num a) => m a
positiveInteger = label "positive integer" do
  digit  <- satisfy isPositiveDigit <?> "nonzero digit"
  digits <- takeWhileP (Just "digit") isDigit
  pure . foldl' (\n d -> 10*n + fromIntegral (digitToInt d)) 0 $ digit : chunkToTokens (Proxy @s) digits

positionalArgument :: (Parser e s m, Num a) => m a
positionalArgument = ifAtLeast POSIXStandard (positiveInteger <* char '$' <?> "positional argument")

flag :: Parser e s m => m Flag
flag = singleCharacter "flag"
  [
  ---- C standard ----
    ('-',  LeftJustify,   CStandard)
  , ('+',  PositivePlus,  CStandard)
  , (' ',  PositiveSpace, CStandard)
  , ('#',  Alternative,   CStandard)
  , ('0',  ZeroPadding,   CStandard)
  ---- POSIX ----
  , ('\'', ThousandsSeparators, POSIXStandard)
  ---- Haskell ----
  , ('=',  RadixPrefix,               Extensions)
  , ('~',  SignedNondecimalSpecifier, Extensions)
  ]

parseSpecifier :: Parser e s m => m Specifier
parseSpecifier = singleCharacter "specifier"
  [
  ---- C(++) standard ----
    ('d', Integer SignedDecimal,                       CStandard)
  , ('i', Integer SignedDecimal,                       CStandard)
  , ('b', Integer . Unsigned $ Binary Lowercase,       CStandard)
  , ('B', Integer . Unsigned $ Binary Uppercase,       CStandard)
  , ('u', Integer $ Unsigned Decimal,                  CStandard)
  , ('o', Integer $ Unsigned Octal,                    CStandard)
  , ('x', Integer . Unsigned $ Hexadecimal Lowercase,  CStandard)
  , ('X', Integer . Unsigned $ Hexadecimal Uppercase,  CStandard)
  , ('f', FloatingPoint DecimalDigits Lowercase,       CStandard)
  , ('F', FloatingPoint DecimalDigits Uppercase,       CStandard)
  , ('e', FloatingPoint DecimalExponent Lowercase,     CStandard)
  , ('E', FloatingPoint DecimalExponent Uppercase,     CStandard)
  , ('g', FloatingPoint DecimalShortest Lowercase,     CStandard)
  , ('G', FloatingPoint DecimalShortest Uppercase,     CStandard)
  , ('a', FloatingPoint HexadecimalExponent Lowercase, CStandard)
  , ('A', FloatingPoint HexadecimalExponent Uppercase, CStandard)
  , ('c', Character,                                   CStandard)
  , ('s', String,                                      CStandard)
  , ('p', Pointer,                                     CStandard)
  , ('%', Percent,                                     CStandard)
    -- Omitted: %n
  ---- Extensions ----
  -- Strings
  , ('S', SpecificString,  Extensions)
  -- Haskell
  , ('?', Showable,        Extensions)
  -- User-extensible formatting
  , ('@', Formattable,     Extensions) -- Could split: one for a -> buffer and one for (flags, length, a) -> buffer
  , ('v', Function Simple, Extensions) -- Lowercase letters are reserved by the C standard; could rework
  , ('V', Function Full,   Extensions)
  -- Buffer
  , ('|', Buffer,          Extensions) -- could change
  ]

intParameter :: Parser e s m => m IntParameter
intParameter =
  Fixed    <$> positiveInteger <|>
  Argument <$  char '*' <*> option InOrder (Positional <$> positionalArgument)

parseWidth :: Parser e s m => m Width
parseWidth = Width <$> intParameter <?> "width"

parsePrecision :: Parser e s m => m Precision
parsePrecision = Precision <$ char '.' <*> intParameter <?> "precision"

-- All in the C standard, for now
parseSimpleLength :: Parser e s m => m SimpleLength
parseSimpleLength = label "simple length modifier" $
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

parseLength :: Parser e s m => m Length
parseLength =   ifAtLeast Extensions (CLength <$ char 'C' <*> optional parseSimpleLength)
            <|> HsLength <$> parseSimpleLength
            <?> "length modifier"

parseRadix :: Parser e s m => m Radix
parseRadix = ifAtLeast Extensions (toRadixErr <$ char 'R' <*> validRadix <?> "radix") where
  validRadix :: Parser e s m => m Int
  validRadix = label "number from 1 to 36" do
    let digit = satisfy isDigit
        tens n next = option n $ (10*n +) . digitToInt <$> next
    digit >>= \case
      '0' -> digitToInt <$> satisfy isPositiveDigit
      '1' -> tens 1 digit
      '2' -> tens 2 digit
      '3' -> tens 3 $ satisfy \c -> fromIntegral @_ @Word (ord c - ord '0') <= 6
      d   -> pure $ digitToInt d

-- %[arg$][flags][width][.precision][length][radix]specifier
formatSpecifier :: Parser e s m => m FormatSpecifier
formatSpecifier = label "format specifier" do
  _              <- char '%'
  argument       <- optional $ try positionalArgument
  flags          <- many flag
  width          <- optional parseWidth
  precision      <- optional parsePrecision
  lengthModifier <- optional parseLength
  radix          <- optional parseRadix
  specifier      <- parseSpecifier
  pure FormatSpecifier{..}

formatStringChunk :: Parser e s m => m (FormatStringChunk (Tokens s))
formatStringChunk =   LiteralChunk         <$> takeWhile1P (Just "non-% character") (/= '%')
                  <|> FormatSpecifierChunk <$> formatSpecifier

parseFormatString :: Parser e s m => m (FormatString (Tokens s))
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
