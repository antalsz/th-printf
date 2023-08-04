module Language.Haskell.Printf.Buffer (UnsizedBuffer (..), Buffer (..), Sized (..)) where

import Data.Char (intToDigit)
import qualified Data.DList as D
import Data.String
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as T

data Sized a = Sized { sizedValue :: !a
                     , sizedSize  :: !Int
                     }
  deriving (Show, Ord, Eq)

instance (IsString a) => IsString (Sized a) where
  fromString s = Sized { sizedValue = fromString s, sizedSize = length s }

instance (Semigroup a) => Semigroup (Sized a) where
  Sized a b <> Sized c d = Sized (a <> c) (b + d)
  {-# INLINE (<>) #-}

instance (Monoid a) => Monoid (Sized a) where
  mempty = Sized { sizedValue = mempty, sizedSize = 0 }
  mappend = (<>)
  {-# INLINE mappend #-}

class (IsString a, Monoid a) => UnsizedBuffer a where
  sText :: S.Text -> a
  sText = fromString . S.unpack
  {-# INLINE sText #-}

  lText :: L.Text -> a
  lText = fromString . L.unpack
  {-# INLINE lText #-}

  singleton :: Char -> a
  singleton = fromString . pure
  {-# INLINE singleton #-}

  digit :: Int -> a
  digit = singleton . intToDigit
  {-# INLINE digit #-}

  cons :: Char -> a -> a
  cons c s = singleton c <> s
  {-# INLINE cons #-}

  repeatN :: Int -> Char -> a
  repeatN n = fromString . replicate n
  {-# INLINE repeatN #-}

class UnsizedBuffer a => Buffer a where
  size :: a -> Int

instance (a ~ Char) => UnsizedBuffer (D.DList a) where
  sText = fromString . S.unpack
  lText = fromString . L.unpack
  singleton = D.singleton
  digit = D.singleton . intToDigit
  cons = D.cons
  repeatN = D.replicate
  {-# INLINE sText #-}
  {-# INLINE lText #-}
  {-# INLINE singleton #-}
  {-# INLINE digit #-}
  {-# INLINE cons #-}
  {-# INLINE repeatN #-}

instance UnsizedBuffer Builder where
  sText = T.fromText
  lText = T.fromLazyText
  singleton = T.singleton
  digit = T.hexadecimal
  cons c buf = T.singleton c <> buf
  repeatN n c = T.fromLazyText . L.replicate (fromIntegral n) $ L.singleton c
  {-# INLINE sText #-}
  {-# INLINE lText #-}
  {-# INLINE singleton #-}
  {-# INLINE digit #-}
  {-# INLINE cons #-}
  {-# INLINE repeatN #-}

instance UnsizedBuffer a => UnsizedBuffer (Sized a) where
  sText = Sized <$> sText <*> S.length
  lText = Sized <$> lText <*> fromIntegral . L.length
  singleton = flip Sized 1 <$> singleton
  digit = flip Sized 1 <$> digit
  cons c (Sized ubuf sz) = Sized (cons c ubuf) (sz + 1)
  repeatN n c = Sized (repeatN n c) n
  {-# INLINE sText #-}
  {-# INLINE lText #-}
  {-# INLINE singleton #-}
  {-# INLINE digit #-}
  {-# INLINE cons #-}
  {-# INLINE repeatN #-}

instance UnsizedBuffer a => Buffer (Sized a) where
  size = sizedSize
  {-# INLINE size #-}
