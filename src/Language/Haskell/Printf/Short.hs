{-|
Like "Language.Haskell.Printf", but with shorter names.
-}
module Language.Haskell.Printf.Short (
  -- * Format string quasiquoters
  f,
  f1,
  s,
  t,
  st,
  lt,
  p,
  hp,
  -- * Type classes
  module Language.Haskell.Printf.ExportTypes
) where

import Language.Haskell.TH.Quote
import Language.Haskell.Printf
import Language.Haskell.Printf.ExportTypes

f, f1, s, t, st, lt, p, hp :: QuasiQuoter
f = printf -- ^ 'printf', the generic formatter
f1 = printf1 -- ^ 'printf1', the generic formatter with an extra argument.
s = sprintf -- ^ 'sprintf', the 'String' formatter
t = tprintf -- ^ 'tprintf', the strict 'Data.Text.Text' formatter
st = stprintf -- ^ 'stprintf', the strict 'Data.Text.Text' formatter
lt = ltprintf -- ^ 'ltprintf', the lazy 'Data.Text.Lazy.Text' formatter
p = pprintf -- ^ 'pprintf', the formatter that prints directly to @stdout@
hp = fprintf
-- ^ 'fprintf', the formatter that prints directly to the 'System.IO.Handle' it gets as its first argument
