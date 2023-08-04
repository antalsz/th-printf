{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.Printf.Buffer (
  UnsizedBuf (..),
  Buf (..),
  Sized (..),
  Buffer,
  PrintfResult (..),
  PrintfResult1 (..)
) where

import Control.Monad.IO.Class
import Data.Char (intToDigit)
import Data.DList (DList())
import qualified Data.DList as D
import Data.Kind (Type)
import Data.String
import qualified Data.Text as S
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as T
import qualified Data.Text.Lazy.IO as LIO
import System.IO (Handle)

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

class (IsString a, Monoid a) => UnsizedBuf a where
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

class UnsizedBuf a => Buf a where
  size :: a -> Int

instance (a ~ Char) => UnsizedBuf (D.DList a) where
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

instance UnsizedBuf Builder where
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

instance UnsizedBuf a => UnsizedBuf (Sized a) where
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

instance UnsizedBuf a => Buf (Sized a) where
  size = sizedSize
  {-# INLINE size #-}

type family Buffer r :: Type

class Buf (Buffer r) => PrintfResult r where
  finalize :: Buffer r -> r

class Buf (Buffer r) => PrintfResult1 r where
  type PrintfParameter r :: Type
  
  finalizeWith :: PrintfParameter r -> Buffer r -> r

type instance Buffer String = Sized (DList Char)
instance PrintfResult String where
  finalize = D.toList . sizedValue
  {-# INLINE finalize #-}

type instance Buffer Text = Sized Builder
instance PrintfResult Text where
  finalize = T.toLazyText . sizedValue
  {-# INLINE finalize #-}

type instance Buffer S.Text = Sized Builder
instance PrintfResult S.Text where
  finalize = L.toStrict . finalize
  {-# INLINE finalize #-}

type instance Buffer (m ()) = Sized Builder
instance MonadIO m => PrintfResult (m ()) where
  finalize = liftIO . LIO.putStr . finalize
  {-# INLINE finalize #-}
instance MonadIO m => PrintfResult1 (m ()) where
  type PrintfParameter (m ()) = Handle
  finalizeWith h = liftIO . LIO.hPutStr h . finalize
  {-# INLINE finalizeWith #-}

type instance Buffer (D.DList Char) = Sized (DList Char)
instance PrintfResult (D.DList Char) where
  finalize = sizedValue
  {-# INLINE finalize #-}

type instance Buffer Builder = Sized Builder
instance PrintfResult Builder where
  finalize = sizedValue
  {-# INLINE finalize #-}

type instance Buffer (Sized (DList Char)) = Sized (DList Char)
instance PrintfResult (Sized (DList Char)) where
  finalize = id
  {-# INLINE finalize #-}

type instance Buffer (Sized Builder) = Sized Builder
instance PrintfResult (Sized Builder) where
  finalize = id
  {-# INLINE finalize #-}
