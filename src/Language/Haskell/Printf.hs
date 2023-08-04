{-# LANGUAGE TemplateHaskell #-}

{- | "Text.Printf" is a useful module, but due to the typeclass hacks it uses, it can
be hard to tell if the format string you wrote is well-formed or not.
This package provides a mechanism to create formatting functions at compile time.

Note that, to maintain consistency with other printf implementations, negative ints
that are printed as unsigned will \"underflow\". (Text.Printf does this too.)

>>> [s|%u|] (-1 :: Int32)
WAS "4294967295"
NOW Not in scope: type constructor or class `Int32'

Thus, any time you want to print a number using the unsigned, octal, or hex specifiers,
your input must be an instance of "Bounded".
-}
module Language.Haskell.Printf (
  -- * Format string quasiquoters
  f,
  f1,
  s,
  t,
  st,
  p,
  hp,
  -- * Type classes
  PrintfString(),
  PrintfResult(),
  PrintfResult1(..),
  PrintfBuffer,
  -- * Build your own quasiquoters
  qqPrintf,
  qqPrintf1,
) where

import Control.Monad.IO.Class
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Language.Haskell.Printf.Lib
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote

import Language.Haskell.Printf.Result (PrintfBuffer, PrintfResult(), PrintfResult1(PrintfParameter))
import Language.Haskell.Printf.String (PrintfString())

{- | @
['s'|Hello, %s! (%d people greeted)|] :: ... -> 'String'
@

This formatter follows the guidelines listed
<http://www.cplusplus.com/reference/cstdio/printf/ here>, except for
@%n@ (store number of printed characters) for obvious
reasons.

@
%c     :: 'Char'
%s     :: 'PrintfString' s => s -- String, lazy text, or strict text
%S     :: 'String'
%q     :: 'Data.Text.Lazy.Text' -- lazy text
%Q     :: 'Data.Text.Text' -- strict text

-- custom formatters
%@     :: ... -> (a -> Buffer r) -> a .... -> r

-- datatypes with Show instances
%?     :: 'Show' a => a

-- signed integer types
%d, %i :: 'Integral' i => i

-- unsigned integer types
%u     :: ('Bounded' i, 'Integral' i) => i
%o     :: ('Bounded' i, 'Integral' i) => i
%x, %X :: ('Bounded' i, 'Integral' i) => i

-- floats
%a, %A :: 'RealFloat' f => f
%e, %E :: 'RealFloat' f => f
%f, %F :: 'RealFloat' f => f
%g, %G :: 'RealFloat' f => f

%p     :: 'Foreign.Ptr.Ptr' a
@
-}
s :: QuasiQuoter
s = qqPrintf [t|String|]

-- | Behaves identically to 's', but produces lazy 'Data.Text.Lazy.Text'.
t :: QuasiQuoter
t = qqPrintf [t|L.Text|]

-- | Behaves identically to 's', but produces strict 'Data.Text.Text'.
st :: QuasiQuoter
st = qqPrintf [t|S.Text|]

{- | Like 's', but prints the resulting string to @stdout@.

@
[p|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => ... -> m ()
@
-}
p :: QuasiQuoter
p = qqPrintf [t|forall m. MonadIO m => m ()|]

{- | Like 'p', but takes as its first argument the 'System.IO.Handle' to print to.

@
[hp|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => 'System.IO.Handle' -> ... -> m ()
@
-}
hp :: QuasiQuoter
hp = qqPrintf1 [t|forall m. MonadIO m => m ()|]

f :: QuasiQuoter
f = toQuasiQuoter Unparameterized Nothing

f1 :: QuasiQuoter
f1 = toQuasiQuoter Parameterized Nothing

qqPrintf :: TypeQ -> QuasiQuoter
qqPrintf = toQuasiQuoter Unparameterized . Just

qqPrintf1 :: TypeQ -> QuasiQuoter
qqPrintf1 = toQuasiQuoter Parameterized . Just
