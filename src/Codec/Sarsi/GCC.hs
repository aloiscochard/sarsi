{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.GCC where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Text (pack)

messageParser :: Parser Message
messageParser = do
  fp <- takeWhile1 (\c -> c /= sepChar && c /= '\n' && c /= '\r') <* char sepChar
  n <- decimal <* char sepChar
  c <- decimal <* char sepChar
  l <-
    choice
      [ string " warning: " *> return Warning,
        string " error: " *> return Error
      ]
  txt <- takeWhile1 (\c -> c /= '\n' && c /= '\r') <* end
  return $ Message (Location {filePath = fp, column = c, line = n}) Error [txt]
  where
    end = choice [const () <$> "\n", endOfInput, return ()]
    sepChar = ':'
