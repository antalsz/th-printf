{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Language.Printf.GADT where

import Control.Monad.IO.Class
import Data.Kind

data Specifier (buf :: Type) (cs :: [Constraint]) (ts :: [Type]) where
  -- Integer        :: Integer  cs i -> Specifier buf cs               '[i]
  -- FloatingPoint  :: Floating cs r -> Specifier buf cs               '[r]
  Character      ::                  Specifier buf '[]              '[Char]
  -- String         :: String   cs s -> Specifier buf cs               '[s]
  -- Pointer        ::                  Specifier buf '[]              '[Ptr a]
  -- Formattable    ::                  Specifier buf '[Formattable a] '[a]
  Showable       ::                  Specifier buf '[Show a]        '[a]
  SimpleFunction ::                  Specifier buf '[]              '[a -> buf, a]
  -- FullFunction   ::                  Specifier buf '[]              '[a... -> buf, a]
  Buffer         ::                  Specifier buf '[]              '[buf]

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data Chunkable = ChunkForbidden | ChunkAllowed deriving (Eq, Ord, Show, Read, Enum, Bounded)

data RawFormatString :: Chunkable -> Type -> [Constraint] -> [Type] -> Type where
  Empty     :: RawFormatString 'ChunkAllowed buf '[] '[]
  Chunk     :: buf
            -> RawFormatString 'ChunkAllowed   buf cs ts
            -> RawFormatString 'ChunkForbidden buf cs ts
  Specifier :: Specifier buf scs sts
            -> RawFormatString chunkable     buf cs          ts
            -> RawFormatString 'ChunkAllowed buf (scs ++ cs) (sts ++ ts)

data FormatString (buf :: Type) (cs :: [Constraint]) (ts :: [Type]) =
  forall chunkable. FormatString (RawFormatString chunkable (PureBuffer buf) cs ts)

type family Constraints (cs :: [Constraint]) :: Constraint where
  Constraints '[]       = ()
  --Constraints '[c]      = c
  Constraints (c ': cs) = (c, Constraints cs)

type family Function (as :: [Type]) (r :: Type) :: Type where
  Function (a ': as) r = a -> Function as r
  Function '[]       r = r

class Buffer buf where
  type PureBuffer buf = (repr :: Type) | repr -> buf

  (+>) :: buf -> buf -> buf
    
  emptyBuffer :: buf

  writeBuffer    :: PureBuffer buf -> buf
  writeCharacter :: Char           -> buf
  writeString    :: String         -> buf

class Buffer (PBuffer p) => Printable p where
  type PBuffer p :: Type

  bufferContents :: PBuffer p -> p

instance Buffer (String -> String) where
  type PureBuffer (String -> String) = String -> String

  (+>) = (.)

  emptyBuffer = id

  writeBuffer = ($)
  writeCharacter = (++) . pure
  writeString = (++)

instance Printable String where
  type PBuffer String = String -> String

  bufferContents = ($ "")

instance MonadIO m => Buffer (m ()) where
  type PureBuffer (m ()) = m ()

  (+>) = (*>)
     
  emptyBuffer = pure ()

  writeBuffer = id
  writeCharacter = liftIO . putChar
  writeString = liftIO . putStr

instance MonadIO m => Printable (m ()) where
  type PBuffer (m ()) = m ()

  bufferContents = id

renderRawFormatString :: forall buf r cs ts chunkable.
                         (Buffer buf, Constraints cs) =>
                         buf -> (buf -> r) -> RawFormatString chunkable (PureBuffer buf) cs ts -> Function ts r
renderRawFormatString !buf k Empty = k buf
renderRawFormatString !buf k (Chunk lit fstr) = renderRawFormatString (buf +> writeBuffer lit) k fstr
renderRawFormatString !buf k (Specifier spec fstr) = case spec of
  Character      -> \c    -> renderRawFormatString (buf +> writeCharacter c)     k fstr
  Showable       -> \x    -> renderRawFormatString (buf +> writeString (show x)) k fstr
  SimpleFunction -> \f x  -> renderRawFormatString (buf +> writeBuffer (f x))    k fstr
  Buffer         -> \buf' -> renderRawFormatString (buf +> writeBuffer buf')     k fstr

kprintf' :: forall buf r cs ts.
            (Buffer buf, Constraints cs) =>
            buf -> (buf -> r) -> FormatString buf cs ts -> Function ts r
kprintf' !buf k (FormatString fstr) = renderRawFormatString buf k fstr

kprintf :: forall buf r cs ts.
           (Buffer buf, Constraints cs) =>
           (buf -> r) -> FormatString buf cs ts -> Function ts r
kprintf = kprintf' emptyBuffer

bprintf :: forall buf cs ts.
           (Buffer buf, Constraints cs) =>
           FormatString buf cs ts -> Function ts buf
bprintf = kprintf id

printf :: forall p cs ts.
          (Printable p, Constraints cs) =>
          FormatString (PBuffer p) cs ts -> Function ts p
printf = kprintf $ bufferContents @p
