{-# LANGUAGE StrictData #-}

-- Unvalidated format strings

module Language.Printf.FormatString where

import Data.String

data Flag = LeftJustify
          | PositivePlus
          | PositiveSpace
          | Alternative
          | ZeroPadding
          | ThousandsSeparators -- POSIX
          | RadixPrefix -- Haskell
          | SignedNondecimalSpecifier -- Haskell
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

data SimpleLength = Byte -- "Char" in C
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

data Length = HsLength SimpleLength
            | CLength (Maybe SimpleLength)
            deriving (Eq, Ord, Show, Read)

data Radix =        R1 |  R2 |  R3 |  R4 |  R5 |  R6 |  R7 |  R8 |  R9
           | R10 | R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19
           | R20 | R21 | R22 | R23 | R24 | R25 | R26 | R27 | R28 | R29
           | R30 | R31 | R32 | R33 | R34 | R35 | R36
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

toRadixErr :: Int -> Radix
toRadixErr = toEnum @Radix . subtract 1

fromRadix :: Radix -> Int
fromRadix = (+ 1) . fromEnum @Radix

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

data FunctionType = Simple | Full
                  -- HS extension
                  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Specifier = Integer IntegerStyle
               | FloatingPoint FloatingStyle Case
               | Character
               | String
               | Pointer
               -- No %n, which would be StoreOutputLength
               | SpecificString -- HS extension
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
  , radix          :: Maybe Radix -- HS extension
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
