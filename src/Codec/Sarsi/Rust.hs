{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.Rust where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import Data.Attoparsec.Text

messageParser :: Parser Message
messageParser = do
  l <-
    choice
      [ string "warning" *> untilSep0 *> string ": " *> return Warning,
        string "error" *> untilSep0 *> string ": " *> return Error
      ]
  body <- untilLineBreak <* "\n"
  fp <- many1 space *> "--> " *> untilSep <* char sepChar
  n <- decimal <* char sepChar
  c <- decimal <* "\n"
  comments <- many' (untilLineBreak <* "\n")
  _ <- end
  return $ Message (Location fp c n) l (body : comments)
  where
    untilSep = takeWhile1 $ \w -> w /= sepChar
    untilSep0 = Data.Attoparsec.Text.takeWhile $ \w -> w /= ':'
    end = choice [const () <$> "\n", endOfInput, return ()]
    sepChar = ':'
    untilLineBreak = takeWhile1 $ \w -> w /= '\n'
