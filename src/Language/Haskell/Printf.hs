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
  printf,
  printf1,
  sprintf,
  tprintf,
  stprintf,
  ltprintf,
  pprintf,
  fprintf,
  -- * Type classes
  module Language.Haskell.Printf.ExportTypes,
  -- * Build your own quasiquoters
  qqPrintf,
  qqPrintf1,
) where

import Control.Monad.IO.Class
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Language.Haskell.Printf.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote

import Language.Haskell.Printf.ExportTypes

{- | @
['printf'|Hello, %s! (%d people greeted)|] :: 'PrintfResult' r => ... -> r
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
%@     :: ... -> (a -> 'PrintfBuffer' r) -> a .... -> r

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
printf :: QuasiQuoter
printf = toQuasiQuoter Unparameterized Nothing

{- | 'printf', but taking an extra argumenbt before the format parameters.  What
this paramter does depends on the result type.

@
['printf'|Hello, %s! (%d people greeted)|] :: 'PrintfResult1' r => 'PrintfParameter' r -> ... -> r
@
-}
printf1 :: QuasiQuoter
printf1 = toQuasiQuoter Parameterized Nothing

-- | 'printf' specialized to produce a 'String'
sprintf :: QuasiQuoter
sprintf = qqPrintf [t|String|]

-- | 'printf' specialized to produce a strict 'Data.Text.Text'.
tprintf :: QuasiQuoter
tprintf = qqPrintf [t|S.Text|]

-- | 'printf' specialized to produce a strict 'Data.Text.Text'.
stprintf :: QuasiQuoter
stprintf = qqPrintf [t|S.Text|]

-- | 'printf' specialized to produce a lazy 'Data.Text.Lazy.Text'.
ltprintf :: QuasiQuoter
ltprintf = qqPrintf [t|L.Text|]

{- | 'printf' specialized to print the result to @stdout@.

@
[printf|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => ... -> m ()
@
-}
pprintf :: QuasiQuoter
pprintf = qqPrintf [t|forall m. MonadIO m => m ()|]

{- | 'printf1' specialized to print the result to the 'System.IO.Handle' it gets as
its first argument.

@
[fprintf|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => 'System.IO.Handle' -> ... -> m ()
@
-}
fprintf :: QuasiQuoter
fprintf = qqPrintf1 [t|forall m. MonadIO m => m ()|]

-- | Generate your own specialized 'printf' quasiquoter that produces a specific type.
qqPrintf :: TypeQ -> QuasiQuoter
qqPrintf = toQuasiQuoter Unparameterized . Just

-- | Generate your own specialized 'printf1' quasiquoter that produces a specific type.
qqPrintf1 :: TypeQ -> QuasiQuoter
qqPrintf1 = toQuasiQuoter Parameterized . Just
