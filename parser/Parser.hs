{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

import Control.Applicative hiding ((<|>))
import Control.Monad.RWS
import Data.Char
import Data.CharSet hiding (map)
import Data.Maybe
import qualified Data.Set as S
import Lens.Micro.Platform
import Parser.Types
import Text.Parsec hiding (many)
import Text.ParserCombinators.ReadP (ReadP (), readP_to_S)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read.Lex (lexChar)

type Warning = String

-- Skips @\&@ and @\ \@ gaps; the former will be skipped after characters by
-- 'lexChar', but not initially, and thus not after gaps
consumeGaps :: ReadP ()
consumeGaps = do
  rest <- ReadP.look
  case rest of
    '\\':'&':_ ->
      ReadP.string "\\&" *> consumeGaps
    '\\':c:_
      | isSpace c ->
          ReadP.string ['\\', c] *> ReadP.munch isSpace *> ReadP.char '\\' *> consumeGaps
    _ -> pure ()

lexStringBody :: ReadP String
lexStringBody = consumeGaps *> ReadP.manyTill (lexChar <* consumeGaps) ReadP.eof

parseStr :: String -> Either ParseError ([Atom], [[Warning]])
parseStr = fmap (unzip . map normalizeAndWarn) . parse printfStr "" . readStringBody
 where
  readStringBody s = case readP_to_S lexStringBody s of
    [(str, "")] -> str
    _ -> error "malformed input"

normalizeAndWarn :: Atom -> (Atom, [Warning])
normalizeAndWarn s@Str{} = (s, [])
normalizeAndWarn (Arg f) = (Arg a, b)
 where
  (_, a, b) = runRWS (warnLength f >> go (spec f)) () f
  go c | c `elem` "aAeEfFgGxXo" = return ()
  go c | c `elem` "b@" = warnLJust >> warnSign >> warnPrefix >> warnZero >> warnSpace
  go c | c `elem` "csSqQ?" = warnSign >> warnPrefix >> warnZero >> warnSpace
  go c | c `elem` "diu" = warnPrefix
  go 'p' = warnSign >> warnPrefix >> warnZero
  go _ = undefined
  warnFlag ::
    (Eq a, MonadWriter [String] m, MonadState FormatArg m) =>
    Lens' FlagSet a ->
    a ->
    a ->
    Char ->
    m ()
  warnFlag lens' bad good flagName = do
    oldVal <- use (flags_ . lens')
    when (oldVal == bad) $ do
      c <- use spec_
      flags_ . lens' .= good
      tell
        [ "`"
            ++ [flagName]
            ++ "` flag has no effect on `"
            ++ [c]
            ++ "` specifier"
        ]
  warnLJust = warnFlag adjustment_ (Just LeftJustified) Nothing '-'
  warnSign = warnFlag signed_ True False '+'
  warnPrefix = warnFlag prefixed_ True False '#'
  warnSpace = warnFlag spaced_ True False ' '
  warnZero = warnFlag adjustment_ (Just ZeroPadded) Nothing '0'
  phonyLengthSpec =
    S.fromList $
      [(x, y) | x <- "diuoxX", y <- ["L"]]
        ++ [ (x, y)
           | x <- "fFeEgGaA"
           , y <- ["hh", "h", "l", "ll", "j", "z", "t"]
           ]
        ++ [(x, y) | x <- "csSqQ?bQ", y <- ["hh", "h", "ll", "j", "z", "t", "L"]]
        ++ map ('p',) ["hh", "h", "l", "ll", "j", "z", "t", "L"]
  warnLength FormatArg{spec, lengthSpec = Just l}
    | (spec, show l) `S.member` phonyLengthSpec =
        tell
          [ "`"
              ++ show l
              ++ "` length modifier has no effect when combined with `"
              ++ [spec]
              ++ "` specifier"
          ]
  warnLength _ = return ()

flagSet :: CharSet
flagSet = fromList "-+ #0"

specSet :: CharSet
specSet = fromList "diuoxXfFeEaAgGpcsSQq?b@"

lengthSpecifiers :: [(String, LengthSpecifier)]
lengthSpecifiers =
  [ ("hh", HH)
  , ("h", H)
  , ("ll", LL)
  , ("l", L)
  , ("j", J)
  , ("z", Z)
  , ("t", T)
  , ("L", BigL)
  ]

oneOfSet :: Stream s m Char => CharSet -> ParsecT s u m Char
oneOfSet s = satisfy (`member` s)

printfStr :: Stream s m Char => ParsecT s u m [Atom]
printfStr =
  many $
    Str "%" <$ try (string "%%")
      <|> Arg <$> fmtArg
      <|> Str <$> some (satisfy (/= '%'))

fmtArg :: Stream s m Char => ParsecT s u m FormatArg
fmtArg = do
  _ <- char '%'
  flags <- do
    fs <- many $ do
      c <- oneOfSet flagSet <?> "flag"
      pure $ case c of
        '-' -> FlagLJust
        '+' -> FlagSigned
        ' ' -> FlagSpaced
        '#' -> FlagPrefixed
        '0' -> FlagZeroPadded
        _ -> error "unreachable"
    let flagSet' = S.fromList fs
    if S.size flagSet' < length fs
      then fail "Duplicate flags specified"
      else pure $ toFlagSet flagSet'
  width <- numArg <?> "width"
  precision <- optionMaybe (char '.' *> numArg) <?> "precision"
               -- XXX warn about width/precision
  lengthSpec <-
    optionMaybe $ choice $ Prelude.map (\(a, b) -> b <$ string a) lengthSpecifiers
  spec <- oneOfSet specSet <?> "valid specifier"
  pure $ FormatArg flags width (fromMaybe (Given 0) <$> precision) spec lengthSpec
 where
  nat = do
    c <- many1 $ satisfy isDigit
    return (read c :: Integer)
  numArg = optionMaybe (Given <$> nat <|> Need <$ char '*')
