module Language.Printf.HaskellString (parseStringBody, lexStringBody) where

import Data.Char
import Text.ParserCombinators.ReadP
import Text.Read.Lex

-- Skips @\&@ and @\ \@ gaps; the former will be skipped after characters by
-- 'lexChar', but not initially, and thus not after gaps
consumeGaps :: ReadP ()
consumeGaps = do
  rest <- look
  case rest of
    '\\':'&':_           -> string "\\&" *> consumeGaps
    '\\':c:_ | isSpace c -> string ['\\', c] *> munch isSpace *> char '\\' *> consumeGaps
    _                    -> pure ()

lexStringBody :: ReadP String
lexStringBody = consumeGaps *> manyTill (lexChar <* consumeGaps) eof

parseStringBody :: String -> Maybe String
parseStringBody str = case readP_to_S lexStringBody str of
  [(str', "")] -> Just str'
  _ -> Nothing
