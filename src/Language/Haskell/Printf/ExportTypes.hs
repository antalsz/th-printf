{- | Types that are useful to export from both "Language.Haskell.Printf" and
"Language.Haskell.Printf.Short".  An implementation detail, not exported.
-}
module Language.Haskell.Printf.ExportTypes (module Types) where

import Language.Haskell.Printf.Result as Types (PrintfBuffer, PrintfResult(), PrintfResult1(PrintfParameter))
import Language.Haskell.Printf.String as Types (PrintfString())
