module Language.Haskell.Printf.String (PrintfString (..)) where

import Data.String
import qualified Data.Text as S
import qualified Data.Text.Lazy as L

import Language.Haskell.Printf.Buffer

-- | Type class for string-like types that can be printed with the @%s@ format specifier.
class PrintfString str where
  fromPrintfString :: Buffer buf => str -> buf

  -- | Like 'take', but for this string-like type; used for width specifiers
  takePrintfString :: Int -> str -> str

instance PrintfString String where
  fromPrintfString = fromString
  takePrintfString = take
  {-# INLINE fromPrintfString #-}
  {-# INLINE takePrintfString #-}

instance PrintfString S.Text where
  fromPrintfString = sText
  takePrintfString = S.take
  {-# INLINE fromPrintfString #-}
  {-# INLINE takePrintfString #-}

instance PrintfString L.Text where
  fromPrintfString = lText
  takePrintfString = L.take . fromIntegral
  {-# INLINE fromPrintfString #-}
  {-# INLINE takePrintfString #-}
