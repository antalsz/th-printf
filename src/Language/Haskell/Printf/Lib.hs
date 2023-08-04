{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Printf.Lib (
  toQuasiQuoter,
  toPrintf,
  toSplices,
  Parameterization(..),
) where

import Data.Maybe
import Data.String (fromString)
import GHC.Generics (Generic())
import Language.Haskell.Printf.Geometry (
  formatOne,
 )
import qualified Language.Haskell.Printf.Printers as Printers
import Language.Haskell.PrintfArg
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Language.Haskell.Printf.Result
import Control.Monad (mapAndUnzipM)
import Parser (parseStr)
import Parser.Types hiding (
  lengthSpec,
  width,
 )

data Parameterization = Unparameterized | Parameterized
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

{- | Takes a format string as input and produces a tuple @(args, outputExpr)@.

This function processes character escapes as they would appear in Haskell source code.
It will emit warnings (or throw an error, as appropriate) when given an invalid format
string.

Use if you wish to leverage @th-printf@ in conjunction with, for example, an existing
logging library.
-}
toSplices :: Parameterization -> Maybe TypeQ -> String -> Q ([Pat], Exp)
toSplices pr motype s' = case parseStr s' of
  Left x -> fail $ show x
  Right (y, warns) -> do
    mapM_ (qReport False) (concat warns)
    (lhss, rhss) <- mapAndUnzipM extractExpr y
    (prvar, finalizer) <- case pr of
      Unparameterized -> pure ([], [|finalize|])
      Parameterized -> do
        param <- newName "fmtParam"
        pure ([param], [|finalizeWith $(varE param)|])
    let rhss' = [|$finalizer $(mconcatExpQ rhss)|]
    rhssTy <- case motype of
      Just otype -> [|$rhss' :: $otype|]
      Nothing    -> rhss'
    return (map VarP $ concat (prvar : lhss), rhssTy)

-- Avoid an extra @... <> mempty@ through an initial case analysis
mconcatExpQ :: [ExpQ] -> ExpQ
mconcatExpQ []       = [|mempty|]
mconcatExpQ xs@(_:_) = foldr1 (\x y -> [|$x <> $y|]) xs

toPrintf :: Parameterization -> Maybe TypeQ -> String -> ExpQ
toPrintf pr mty s' = uncurry LamE <$> toSplices pr mty s'

toQuasiQuoter :: Parameterization -> Maybe TypeQ -> QuasiQuoter
toQuasiQuoter pr mty =
  QuasiQuoter
    { quoteExp = toPrintf pr mty
    , quotePat = error "this quoter cannot be used in a pattern context"
    , quoteType = error "this quoter cannot be used in a type context"
    , quoteDec = error "this quoter cannot be used in a declaration context"
    }

formatter :: Char -> Q (Maybe Name, ExpQ)
formatter '@' = do
  fmt <- newName "formatFn"
  pure (Just fmt, [|Printers.printfApply $(varE fmt)|])
formatter c = pure . (Nothing,) $ case c of
  's' -> [|Printers.printfGenericString|]
  'S' -> [|Printers.printfString|]
  'q' -> [|Printers.printfLazyText|]
  'Q' -> [|Printers.printfStrictText|]
  '?' -> [|Printers.printfShow|]
  'b' -> [|Printers.printfBuf|]
  'd' -> [|Printers.printfDecimal|]
  'i' -> [|Printers.printfDecimal|]
  'p' -> [|Printers.printfPtr|]
  'c' -> [|Printers.printfChar|]
  'u' -> [|Printers.printfUnsigned|]
  'x' -> [|Printers.printfHex False|]
  'X' -> [|Printers.printfHex True|]
  'o' -> [|Printers.printfOctal|]
  'f' -> [|Printers.printfFloating False|]
  'F' -> [|Printers.printfFloating True|]
  'e' -> [|Printers.printfScientific False|]
  'E' -> [|Printers.printfScientific True|]
  'g' -> [|Printers.printfGeneric False|]
  'G' -> [|Printers.printfGeneric True|]
  'a' -> [|Printers.printfFloatHex False|]
  'A' -> [|Printers.printfFloatHex True|]
  _   -> error $ "Unknown formatting character " ++ show c

extractExpr :: Atom -> Q ([Name], ExpQ)
extractExpr (Str s') = return ([], [|fromString $(stringE s')|])
extractExpr (Arg (FormatArg flags' width' precision' spec' lengthSpec')) = do
  (warg, wexp) <- extractArgs width'
  (parg, pexp) <- extractArgs precision'
  (farg, fexp) <- formatter spec'
  varg <- newName "arg"
  return
    ( catMaybes [warg, parg, farg, Just varg]
    , [| formatOne
           ($fexp
              PrintfArg
                { flagSet = $(lift flags')
                , width = $(wexp)
                , prec = $(pexp)
                , value = $(varE varg)
                , lengthSpec = $(lift lengthSpec')
                , fieldSpec = $(lift spec')
                })
      |]
    )
 where
  extractArgs n = case n of
    Just Need -> do
      a <- newName "arg"
      pure (Just a, [|Just (fromInteger (fromIntegral $(varE a)))|])
    Just (Given n') -> pure (Nothing, [|Just $(litE $ integerL n')|])
    Nothing -> pure (Nothing, [|Nothing|])
