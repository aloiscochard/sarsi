{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.Nix where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Text (pack)

messageParser :: Parser Message
messageParser = do
  _ <- string "error: "
  txt <- manyTill' anyChar (lookAhead $ string " at ")
  _ <- string " at "
  fp <- takeWhile1 (\c -> c /= sepChar && c /= '\n' && c /= '\r') <* char sepChar
  n <- decimal <* char sepChar
  c <- decimal <* end
  return $ Message (Location {filePath = fp, column = c, line = n}) Error [pack txt]
  where
    end = choice [const () <$> "\n", endOfInput, return ()]
    sepChar = ':'
