{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Text.Printf.TH
    ( -- * Formatting strings
      s
    , st
    , sb
      -- * Printing strings
    , hp
    , p
    ) where

import Control.Exception (Exception, throw)
import Control.Monad.IO.Class
import Data.ByteString.UTF8 (fromString)
import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Text (pack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.IO
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding ((<$>), (<>), empty, line)
import Text.Trifecta

import Text.Printf.TH.Parser
import Text.Printf.TH.Printer
import Text.Printf.TH.Printer.String ()
import Text.Printf.TH.Types

data ParseError =
    ParseError
    deriving (Show)

instance Exception ParseError

-- * Formatting strings
-- | @
-- ['s'|Hello, %s! (%d people greeted)|] :: ... -> String
-- @
--
-- This formatter follows the guidelines listed
-- <http://www.cplusplus.com/reference/cstdio/printf/ here>, except for
-- @%n@ (store number of printed characters) for obvious
-- reasons.
--
-- @
-- %c     :: 'Char'
-- %s     :: 'String'
-- %t     :: 'Data.Text.Text'
--
-- %?     :: 'Show' a => a
--
-- %u     :: 'Numeric.Natural.Natural'
--
-- %d, %i :: 'Integral' i => i
-- %o     :: 'Integral' i => i
-- %x, %X :: 'Integral' i => i
--
-- %a, %A :: 'RealFloat' f => f
-- %e, %E :: 'RealFloat' f => f
-- %f, %F :: 'RealFloat' f => f
-- %g, %G :: 'RealFloat' f => f
--
-- %p     :: 'Foreign.Ptr.Ptr' a
-- @
s :: QuasiQuoter
s = quoter 'id

-- | @
-- ['st'|Hello, %s! (%d people greeted)|] :: ... -> 'Data.Text.Text'
-- @
st :: QuasiQuoter
st = quoter 'pack

-- | @
-- ['sb'|Hello, %s! (%d people greeted)|] :: ... -> 'Data.ByteString.ByteString'
-- @
--
-- The resulting string is UTF8-encoded.
sb :: QuasiQuoter
sb = quoter 'fromString

-- | @
-- ['hp'|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => 'Handle' -> ... -> m ()
-- @
--
-- Prints the produced string to the provided 'Handle'. Like C printf, newline is not
-- appended.
hp :: QuasiQuoter
hp =
    QuasiQuoter
        { quoteExp =
              \s -> do
                  LamE pats body <- parse 'id s
                  arg <- newName "arg"
                  ps <- [|liftIO . hPutStr $(varE arg)|]
                  pure $ LamE ((VarP arg) : pats) $ AppE ps body
        , quotePat = error "printf cannot be used in a pattern context"
        , quoteType = error "printf cannot be used in a type context"
        , quoteDec = error "printf cannot be used in a decl context"
        }

-- | @
-- ['p'|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => ... -> m ()
-- @
--
-- @
-- [p|...|] arg1 arg2...
-- @
--
-- is equivalent to
--
-- @
-- [hp|...|] 'System.IO.stdout' arg1 arg2...
-- @
p :: QuasiQuoter
p =
    QuasiQuoter
        { quoteExp =
              \s -> do
                  LamE pats body <- parse 'id s
                  ps <- [|liftIO . putStr|]
                  pure $ LamE pats $ AppE ps body
        , quotePat = error "printf cannot be used in a pattern context"
        , quoteType = error "printf cannot be used in a type context"
        , quoteDec = error "printf cannot be used in a decl context"
        }

quoter f =
    QuasiQuoter
        { quoteExp = parse f
        , quotePat = error "printf cannot be used in a pattern context"
        , quoteType = error "printf cannot be used in a type context"
        , quoteDec = error "printf cannot be used in a decl context"
        }

parse f s =
    case parseString parseFmtStr mempty s of
        Success fmtStr -> build f ''String fmtStr
        Failure xs -> do
            runIO $ displayIO stderr $ renderPretty 0.8 80 $ _errDoc xs
            throw ParseError

build f outputType fmtStr = do
    pairs <- mapM mkExpr fmtStr
    let (args, exprs) = unzip pairs
    lamE (map varP $ concat args) $
        appE (varE f) $
        appsE [[|finalize|], [|Proxy :: Proxy $(conT outputType)|], listE exprs]

mkExpr (Str s) = pure ([] :: [Name], [|valOf (literal $(stringE s))|])
mkExpr (Arg (FormatArg flags width precision spec)) = do
    (warg, wexp) <- extractArgs width
    (parg, pexp) <- extractArgs precision
    varg <- newName "arg"
    pure
        ( catMaybes [warg, parg, Just varg]
        , appE
              formatter
              [|ArgSpec
                    { flagSet = $(lift flags)
                    , width = $(wexp)
                    , prec = $(pexp)
                    , value = $(varE varg)
                    }|])
  where
    extractArgs n =
        case n of
            Just Need -> do
                a <- newName "arg"
                pure (Just a, [|Just $(varE a)|])
            Just (Given n) -> pure (Nothing, [|Just $(litE $ integerL n)|])
            Nothing -> pure (Nothing, [|Nothing|])
    formatter =
        case spec of
            x
                | x `elem` "id" -> [|formatDec|]
            'u' -> [|formatNat|]
            'o' -> [|formatOct|]
            'x' -> [|formatHex|]
            'X' -> [|formatHexUpper|]
            's' -> [|formatStr|]
            't' -> [|formatText|]
            'f' -> [|formatFloat|]
            'F' -> [|formatFloat|]
            'a' -> [|formatHexFloat|]
            'A' -> [|formatHexFloatUpper|]
            'e' -> [|formatSci|]
            'E' -> [|formatSciUpper|]
            'g' -> [|formatG|]
            'G' -> [|formatGUpper|]
            'p' -> [|formatPtr|]
            'c' -> [|formatChar|]
            '?' -> [|formatShowable|]
            _ -> error "???"
