{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module Language.Haskell.Printf.String (PrintfString (..), SomePrintfString (..)) where

import Data.String
import qualified Data.Text as S
import qualified Data.Text.Lazy as L

import Language.Haskell.Printf.Buffer (UnsizedBuf (lText, sText))

-- | The three string-like types that @%s@ natively understands.
data SomePrintfString
  = PString !String
  | PStrictText !S.Text
  | PLazyText !L.Text
  deriving (Eq, Ord, Show, Read)

-- | Type class for string-like types that can be printed with the @%s@ format specifier.
class PrintfString str where
  genStr :: UnsizedBuf buf => str -> buf
  genStr gs = case toPrintfString gs of
    PString s -> fromString s
    PStrictText st -> sText st
    PLazyText lt -> lText lt

  -- | Like 'take', but for this string-like type; used for width specifiers
  genTake :: Int -> str -> str

  -- | Convert a value to its most natural string-like type
  toPrintfString :: str -> SomePrintfString

instance PrintfString SomePrintfString where
  genTake n p = case p of
    PString s -> PString (take n s)
    PStrictText st -> PStrictText (S.take n st)
    PLazyText lt -> PLazyText (L.take (fromIntegral n) lt)

  toPrintfString = id

instance PrintfString String where
  genStr = fromString
  genTake = take
  toPrintfString = PString

instance PrintfString S.Text where
  genStr = sText
  genTake = S.take
  toPrintfString = PStrictText

instance PrintfString L.Text where
  genStr = lText
  genTake = L.take . fromIntegral
  toPrintfString = PLazyText
