{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  s,
  t,
  st,
  p,
  hp,
  f,
  f1,
  printf,
  printf1,
  PrintfString()
) where

import Control.Monad.IO.Class
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Language.Haskell.Printf.Lib
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

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
s = printf [t|String|]

-- | Behaves identically to 's', but produces lazy 'Data.Text.Lazy.Text'.
t :: QuasiQuoter
t = printf [t|L.Text|]

-- | Behaves identically to 's', but produces strict 'Data.Text.Text'.
st :: QuasiQuoter
st = printf [t|S.Text|]

{- | Like 's', but prints the resulting string to @stdout@.

@
[p|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => ... -> m ()
@
-}
p :: QuasiQuoter
p = printf [t|forall m. MonadIO m => m ()|]

{- | Like 'p', but takes as its first argument the 'System.IO.Handle' to print to.

@
[hp|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => 'System.IO.Handle' -> ... -> m ()
@
-}
hp :: QuasiQuoter
hp = printf1 [t|forall m. MonadIO m => m ()|]

f :: QuasiQuoter
f = printf' Unparameterized Nothing

f1 :: QuasiQuoter
f1 = printf' Parameterized Nothing

printf' :: Parameterization -> Maybe TypeQ -> QuasiQuoter
printf' pr mty =
  QuasiQuoter
    { quoteExp = \s' -> do
        (lhss, rhs) <- toSplices s' pr mty
        return $ LamE lhss rhs
    , quotePat = error "this quoter cannot be used in a pattern context"
    , quoteType = error "this quoter cannot be used in a type context"
    , quoteDec = error "this quoter cannot be used in a declaration context"
    }

printf :: TypeQ -> QuasiQuoter
printf = printf' Unparameterized . Just

printf1 :: TypeQ -> QuasiQuoter
printf1 = printf' Parameterized . Just
