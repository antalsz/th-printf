{-# LANGUAGE StrictData #-}

-- Unvalidated format strings

module Language.Printf.FormatString where

import Data.String

data PositiveSign = Plus | Space
                  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Flag = LeftJustify
          | Signed PositiveSign
          | Alternative
          | ZeroPadding
          | ThousandsSeparators -- POSIX
          deriving (Eq, Ord, Show, Read)

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
