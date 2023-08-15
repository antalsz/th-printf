{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Unvalidated format strings

module Language.Printf.FormatString where

import Control.Monad
import Data.Array.IArray
import Data.Char
import Data.Coerce
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup
import Data.String
import Data.Type.Bool
import GHC.Exts (IsList(..))
import GHC.Generics hiding (R1)
import qualified GHC.Generics as G
import Test.QuickCheck hiding (Fixed(..))

--------------------------------------------------------------------------------
-- Types

data Flag = LeftJustify
          | PositivePlus
          | PositiveSpace
          | Alternative
          | ZeroPadding
          | ThousandsSeparators -- POSIX
          | RadixPrefix -- Haskell
          deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

data Argument = InOrder
              | Positional Word -- POSIX
              deriving (Eq, Ord, Show, Read, Generic)

data Parameter a = Fixed a
                 | Argument Argument
                 deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic)

newtype Width = Width (Parameter Word)
  deriving (Eq, Ord, Show, Read, Generic)

newtype Precision = Precision (Parameter Word)
  deriving (Eq, Ord, Show, Read, Generic)

data IntWidthPrecision = Exact | Fastest
                       deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
                       deriving Exemplars via Generically IntWidthPrecision

data DecimalWidth = D32 | D64 | D128
                  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
                  deriving Exemplars via Generically DecimalWidth

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
                  deriving (Eq, Ord, Show, Read, Generic)
                  deriving Exemplars via Generically SimpleLength

data Length = HsLength SimpleLength
            | CLength (Maybe SimpleLength)
            deriving (Eq, Ord, Show, Read, Generic)
            deriving Exemplars via Generically Length

data Radix =        R1 |  R2 |  R3 |  R4 |  R5 |  R6 |  R7 |  R8 |  R9
           | R10 | R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19
           | R20 | R21 | R22 | R23 | R24 | R25 | R26 | R27 | R28 | R29
           | R30 | R31 | R32 | R33 | R34 | R35 | R36
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

toRadixErr :: Int -> Radix
toRadixErr = toEnum @Radix . subtract 1

fromRadix :: Radix -> Int
fromRadix = (+ 1) . fromEnum @Radix

data Case = Lowercase | Uppercase
          deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
          deriving Exemplars via Generically Case

data SignedIntegerBase = Decimal
                       | SGeneral Case -- This distinction is HS extension, as is Uppercase
                       deriving (Eq, Ord, Show, Read, Generic)
                       deriving Exemplars via Generically SignedIntegerBase

data UnsignedIntegerBase = Binary Case -- C24
                         | UGeneral Case -- Uppercase is HS extension
                         | Octal
                         | Hexadecimal Case
                         deriving (Eq, Ord, Show, Read, Generic)
                         deriving Exemplars via Generically UnsignedIntegerBase

data IntegerStyle = Signed SignedIntegerBase
                  | Unsigned UnsignedIntegerBase
                  deriving (Eq, Ord, Show, Read, Generic)
                  deriving Exemplars via Generically IntegerStyle

data FloatingStyle = DecimalDigits
                   | DecimalExponent
                   | DecimalShortest
                   | HexadecimalExponent
                   deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
                   deriving Exemplars via Generically FloatingStyle

data FunctionType = Simple | Full
                  -- HS extension
                  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
                  deriving Exemplars via Generically FunctionType

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
               deriving (Eq, Ord, Show, Read, Generic)
               deriving Exemplars via Generically Specifier

data FormatSpecifier = FormatSpecifier
  { argument       :: Maybe Word
  , flags          :: [Flag]
  , width          :: Maybe Width
  , precision      :: Maybe Precision
  , lengthModifier :: Maybe Length
  , radix          :: Maybe (Parameter Radix) -- HS extension
  , specifier      :: Specifier
  }
  deriving (Eq, Ord, Show, Read, Generic)

data FormatStringChunk str = LiteralChunk str
                           | FormatSpecifierChunk FormatSpecifier
                           deriving (Eq, Ord, Show, Read, Generic)

instance IsString str => IsString (FormatStringChunk str) where
  fromString = LiteralChunk . fromString

newtype FormatString str = FormatString [FormatStringChunk str]
  deriving (Eq, Ord, Show, Read, Generic, Semigroup, Monoid)

--------------------------------------------------------------------------------
-- Pretty-printing format strings

class ShowFormatString a out where
  showFormatString :: a -> out

instance IsString str => ShowFormatString Word str where
  showFormatString = fromString . show

instance (ShowFormatString a str, IsString str) => ShowFormatString (Maybe a) str where
  showFormatString = maybe "" showFormatString

instance ShowFormatString Flag Char where
  showFormatString LeftJustify         = '-'
  showFormatString PositivePlus        = '+'
  showFormatString PositiveSpace       = ' '
  showFormatString Alternative         = '#'
  showFormatString ZeroPadding         = '0'
  showFormatString ThousandsSeparators = '\''
  showFormatString RadixPrefix         = '='

showPositional :: (IsString str, Semigroup str) => Word -> str
showPositional n = fromString (show n) <> "$"

instance (IsString str, Semigroup str) => ShowFormatString Argument str where
  showFormatString InOrder        = ""
  showFormatString (Positional n) = showPositional n

instance (ShowFormatString a str, IsString str, Semigroup str) => ShowFormatString (Parameter a) str where
  showFormatString (Fixed fx)     = showFormatString fx
  showFormatString (Argument arg) = "*" <> showFormatString arg

instance (IsString str, Semigroup str) => ShowFormatString Width str where
  showFormatString (Width w) = showFormatString w

instance (IsString str, Semigroup str) => ShowFormatString Precision str where
  showFormatString (Precision p) = "." <> showFormatString p

instance IsString str => ShowFormatString IntWidthPrecision str where
  showFormatString Exact   = ""
  showFormatString Fastest = "f"

instance IsString str => ShowFormatString DecimalWidth str where
  showFormatString D32  = "H"
  showFormatString D64  = "D"
  showFormatString D128 = "DD"

instance (IsString str, Semigroup str) => ShowFormatString SimpleLength str where
  showFormatString Byte                 = "hh"
  showFormatString Short                = "h"
  showFormatString Long                 = "l"
  showFormatString LongLong             = "ll"
  showFormatString IntMax               = "j"
  showFormatString Size                 = "z"
  showFormatString PtrDiff              = "t"
  showFormatString (IntWithWidth ep wd) = "w" <> showFormatString ep <> fromString (show wd)
  showFormatString LongDouble           = "L"
  showFormatString (DecimalFloat dw)    = showFormatString dw

instance (IsString str, Semigroup str) => ShowFormatString Length str where
  showFormatString (HsLength l)  = showFormatString l
  showFormatString (CLength  ol) = "C" <> showFormatString ol

instance IsString str => ShowFormatString Radix str where
  showFormatString = fromString . show . fromRadix

instance ShowFormatString Case (Char -> Char) where
  showFormatString Lowercase = toLower
  showFormatString Uppercase = toUpper

instance ShowFormatString SignedIntegerBase Char where
  showFormatString Decimal      = 'd'
  showFormatString (SGeneral c) = showFormatString c 'i'

instance ShowFormatString UnsignedIntegerBase Char where
  showFormatString (Binary cs)      = showFormatString cs 'b'
  showFormatString (UGeneral cs)    = showFormatString cs 'u'
  showFormatString Octal            = 'o'
  showFormatString (Hexadecimal cs) = showFormatString cs 'x'

instance ShowFormatString IntegerStyle Char where
  showFormatString (Signed   sib) = showFormatString sib
  showFormatString (Unsigned uib) = showFormatString uib

instance ShowFormatString FloatingStyle Char where
  showFormatString DecimalDigits       = 'f'
  showFormatString DecimalExponent     = 'e'
  showFormatString DecimalShortest     = 'g'
  showFormatString HexadecimalExponent = 'a'

instance ShowFormatString FunctionType Char where
  showFormatString Simple = 'v'
  showFormatString Full   = 'V'

instance ShowFormatString Specifier Char where
  showFormatString (Integer is)          = showFormatString is
  showFormatString (FloatingPoint fs cs) = showFormatString cs (showFormatString fs :: Char)
  showFormatString Character             = 'c'
  showFormatString String                = 's'
  showFormatString Pointer               = 'p'
  showFormatString SpecificString        = 'S'
  showFormatString Formattable           = '@'
  showFormatString Showable              = '?'
  showFormatString (Function ft)         = showFormatString ft
  showFormatString Buffer                = '|'
  showFormatString Percent               = '%'

instance (IsString str, Semigroup str) => ShowFormatString FormatSpecifier str where
  showFormatString FormatSpecifier{argument, flags, width, precision, lengthModifier, radix, specifier} =
    "%"                                          <>
    maybe "" showPositional argument             <>
    fromString (map showFormatString flags)      <>
    showFormatString width                       <>
    showFormatString precision                   <>
    showFormatString lengthModifier              <>
    maybe "" (("R" <>) . showFormatString) radix <>
    fromString [showFormatString specifier]

instance (IsString str, IsList str, Item str ~ Char, Semigroup str) =>
         ShowFormatString (FormatStringChunk str) str where
  showFormatString (LiteralChunk str)          = fromList . concatMap (\case '%' -> "%%" ; c -> [c]) $ toList str
  showFormatString (FormatSpecifierChunk spec) = showFormatString spec

instance (IsString str, IsList str, Item str ~ Char, Semigroup str) =>
         ShowFormatString (FormatString str) str where
  showFormatString (FormatString []) = ""
  showFormatString (FormatString (chunk:chunks)) = sconcat $ showFormatString <$> chunk:|chunks

--------------------------------------------------------------------------------
-- QuickCheck

listElementsArray :: IArray a e => [e] -> a Int e
listElementsArray xs = listArray (0, length xs - 1) xs

arraySize :: (IArray a e, Ix i) => a i e -> Int
arraySize = rangeSize . bounds

-- Not exported
class Exemplars a where
  type Total a :: Bool
  exemplars :: Array Int a

instance Exemplars (V1 p) where
  type Total (V1 p) = 'True
  exemplars = listElementsArray []

instance Exemplars (U1 p) where
  type Total (U1 p) = 'True
  exemplars = listElementsArray [U1]

instance (Exemplars (f p), Exemplars (g p)) => Exemplars ((f :*: g) p) where
  type Total ((f :*: g) p) = Total (f p) && Total (g p)
  exemplars =
    let fs = exemplars
        gs = exemplars
    in listArray (0, arraySize fs * arraySize gs - 1) $ (:*:) <$> elems fs <*> elems gs

instance (Exemplars (f p), Exemplars (g p)) => Exemplars ((f :+: g) p) where
  type Total ((f :+: g) p) = Total (f p) && Total (g p)
  exemplars =
    let fs = exemplars
        gs = exemplars
    in listArray (0, arraySize fs + arraySize gs - 1) $ (L1 <$> elems fs) ++ (G.R1 <$> elems gs)

instance Exemplars (f p) => Exemplars (M1 i c f p) where
  type Total (M1 i c f p) = Total (f p)
  exemplars = M1 <$> exemplars

instance Exemplars a => Exemplars (K1 i a p) where
  type Total (K1 i a p) = Total a
  exemplars = K1 <$> exemplars

instance (Generic a, Exemplars (Rep a ())) => Exemplars (Generically a) where
  type Total (Generically a) = Total (Rep a ())
  exemplars = Generically . to <$> exemplars @(Rep a ())

newtype WidthPrecisionWordExemplars = WidthPrecisionWordExemplars Word

instance Exemplars WidthPrecisionWordExemplars where
  type Total WidthPrecisionWordExemplars = 'False
  exemplars = listArray (0, 9) . coerce @[Word] $ [1..8] ++ [10] ++ [999]

-- For 'SimpleLength' integer widths
instance Exemplars Word where
  type Total Word = 'False
  exemplars = listArray (0, 5) $ 1 : [2^(3+i :: Int) | i <- [0..3]] ++ [100]

deriving via Generically (Maybe a) instance Exemplars a => Exemplars (Maybe a)

instance Exemplars [Flag] where
  type Total [Flag] = 'False
  exemplars = listElementsArray . sort
            $ filterM (const [False, True]) ([minBound..maxBound] ++ [minBound..maxBound])


instance Arbitrary Flag where
  arbitrary = arbitraryBoundedEnum

  shrink LeftJustify         = [ZeroPadding]
  shrink PositivePlus        = [PositiveSpace]
  shrink PositiveSpace       = []
  shrink Alternative         = [RadixPrefix]
  shrink ZeroPadding         = []
  shrink ThousandsSeparators = []
  shrink RadixPrefix         = []
  
instance Arbitrary Argument where
  arbitrary = frequency [ (1, pure InOrder)
                        , (3, Positional . coerce <$> arbitrary @(NonZero (Small Word))) ]
  
  shrink InOrder        = []
  shrink (Positional p) = InOrder : (Positional <$> coerce (shrink @(NonZero (Small Word))) p)

instance Arbitrary1 Parameter where
  liftArbitrary fixed = frequency [ (3, Fixed <$> fixed)
                                  , (1, Argument <$> arbitrary) ]

  liftShrink fixed (Fixed fx)     = Fixed <$> fixed fx
  liftShrink _     (Argument arg) = Argument <$> shrink arg

instance Arbitrary a => Arbitrary (Parameter a) where
  arbitrary = liftArbitrary arbitrary
  shrink    = liftShrink shrink

deriving via Parameter (NonZero (Small Word)) instance Arbitrary Width

deriving via Parameter (NonZero (Small Word)) instance Arbitrary Precision

instance Arbitrary IntWidthPrecision where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary DecimalWidth where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

arbitraryFromArray :: (IArray a e, Ix i) => a i e -> (e -> (Int, Gen e)) -> Gen e
arbitraryFromArray arr mk = frequency $ mk <$> elems arr

indexOfOrAbove :: (Show i, IArray a e, Ix i, Integral i, Ord e) => a i e -> e -> i
indexOfOrAbove arr tgt = uncurry go $ bounds arr where
  go low high
    | low > high = low
    | otherwise  =
      let mid = low + (high - low) `quot` 2 in
      case tgt `compare` (arr ! mid) of
        LT -> go low (mid-1)
        EQ -> mid
        GT -> go (mid+1) high

shrinkFromSortedArray :: (Show i, IArray a e, Ix i, Arbitrary i, Integral i, Ord e) => a i e -> e -> [e]
shrinkFromSortedArray arr =
  map (arr !) . takeWhile (>= fst (bounds arr)) . shrink . indexOfOrAbove arr

arbitraryExemplar :: Exemplars a => (a -> (Int, Gen a)) -> Gen a
arbitraryExemplar = arbitraryFromArray exemplars

arbitraryUniformExemplar :: Exemplars a => Gen a
arbitraryUniformExemplar = arbitraryFromArray exemplars \x -> (1, pure x)

shrinkExemplar :: (Exemplars a, Ord a) => a -> [a]
shrinkExemplar = shrinkFromSortedArray exemplars

lengthFrequency :: Num a => SimpleLength -> (a, Gen SimpleLength)
lengthFrequency sl = case sl of
  IntWithWidth ef i | i == 1 || i == 100 -> (1, IntWithWidth ef . coerce <$> arbitrary @(NonZero (Small Word)))
  _ -> (3, pure sl)

instance Arbitrary SimpleLength where
  arbitrary = arbitraryExemplar lengthFrequency
  shrink    = shrinkExemplar

instance Arbitrary Length where
  arbitrary = arbitraryExemplar \case
                HsLength sl       -> fmap HsLength <$> lengthFrequency sl
                CLength Nothing   -> (3, pure $ CLength Nothing)
                CLength (Just sl) -> fmap (CLength . Just) <$> lengthFrequency sl

  shrink = shrinkExemplar

instance Arbitrary Radix where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Case where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary SignedIntegerBase where
  arbitrary = arbitraryUniformExemplar
  shrink    = shrinkExemplar

instance Arbitrary UnsignedIntegerBase where
  arbitrary = arbitraryUniformExemplar
  shrink    = shrinkExemplar

instance Arbitrary IntegerStyle where
  arbitrary = arbitraryUniformExemplar
  shrink    = shrinkExemplar

instance Arbitrary FloatingStyle where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary FunctionType where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Specifier where
  arbitrary = arbitraryUniformExemplar
  shrink    = shrinkExemplar

instance Arbitrary FormatSpecifier where
  arbitrary = do
    argument       <- coerce $ arbitrary @(Maybe (NonZero (Small Word)))
    flags          <- arbitrary
    width          <- arbitrary
    precision      <- arbitrary
    lengthModifier <- arbitrary
    radix          <- arbitrary
    specifier      <- arbitrary
    pure FormatSpecifier{..}

  shrink fs@FormatSpecifier{argument, flags, width, precision, lengthModifier, radix, specifier} =
    [fs{argument       = argument'}       | argument'       <- shrink argument]       ++
    [fs{flags          = flags'}          | flags'          <- shrink flags]          ++
    [fs{width          = width'}          | width'          <- shrink width]          ++
    [fs{precision      = precision'}      | precision'      <- shrink precision]      ++
    [fs{lengthModifier = lengthModifier'} | lengthModifier' <- shrink lengthModifier] ++
    [fs{radix          = radix'}          | radix'          <- shrink radix]          ++
    [fs{specifier      = specifier'}      | specifier'      <- shrink specifier]

instance (IsString str, Arbitrary str) => Arbitrary (FormatStringChunk str) where
  arbitrary = oneof [ LiteralChunk . fromString <$> frequency [ (3, getPrintableString <$> arbitrary)
                                                              , (1, getUnicodeString <$> arbitrary) ]
                    , FormatSpecifierChunk <$> arbitrary ]

  shrink (LiteralChunk str) = LiteralChunk <$> shrink str
  shrink (FormatSpecifierChunk spec) =
    [LiteralChunk "", LiteralChunk . fromString $ showFormatString spec] ++
    (FormatSpecifierChunk <$> shrink spec)

deriving instance (IsString str, Arbitrary str) => Arbitrary (FormatString str)
