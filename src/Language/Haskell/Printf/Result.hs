{-# LANGUAGE TypeFamilies #-}

module Language.Haskell.Printf.Result (PrintfBuffer, PrintfResult (..), PrintfResult1 (..)) where

import Control.Monad.IO.Class
import Data.DList (DList())
import qualified Data.DList as D
import Data.Kind (Type)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as LIO
import System.IO (Handle)

import Language.Haskell.Printf.Buffer

type family PrintfBuffer r :: Type

class Buffer (PrintfBuffer r) => PrintfResult r where
  finalize :: PrintfBuffer r -> r

class Buffer (PrintfBuffer r) => PrintfResult1 r where
  type PrintfParameter r :: Type
  
  finalizeWith :: PrintfParameter r -> PrintfBuffer r -> r

type instance PrintfBuffer String = Sized (DList Char)
instance PrintfResult String where
  finalize = D.toList . sizedValue
  {-# INLINE finalize #-}

type instance PrintfBuffer L.Text = Sized Builder
instance PrintfResult L.Text where
  finalize = T.toLazyText . sizedValue
  {-# INLINE finalize #-}

type instance PrintfBuffer S.Text = Sized Builder
instance PrintfResult S.Text where
  finalize = L.toStrict . finalize
  {-# INLINE finalize #-}

type instance PrintfBuffer (m ()) = Sized Builder
instance MonadIO m => PrintfResult (m ()) where
  finalize = liftIO . LIO.putStr . finalize
  {-# INLINE finalize #-}
instance MonadIO m => PrintfResult1 (m ()) where
  type PrintfParameter (m ()) = Handle
  finalizeWith h = liftIO . LIO.hPutStr h . finalize
  {-# INLINE finalizeWith #-}

type instance PrintfBuffer (D.DList Char) = Sized (DList Char)
instance PrintfResult (D.DList Char) where
  finalize = sizedValue
  {-# INLINE finalize #-}

type instance PrintfBuffer Builder = Sized Builder
instance PrintfResult Builder where
  finalize = sizedValue
  {-# INLINE finalize #-}

type instance PrintfBuffer (Sized (DList Char)) = Sized (DList Char)
instance PrintfResult (Sized (DList Char)) where
  finalize = id
  {-# INLINE finalize #-}

type instance PrintfBuffer (Sized Builder) = Sized Builder
instance PrintfResult (Sized Builder) where
  finalize = id
  {-# INLINE finalize #-}
