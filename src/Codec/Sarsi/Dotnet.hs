{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.Dotnet where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import Data.Attoparsec.Text
import qualified Data.Text as Text

messageParser :: Parser Message
messageParser = do
  fp <- "/" >> untilChar '('
  n <- decimal <* ","
  c <- decimal <* "):"
  _ <- char ' '
  l <-
    choice
      [ string "warning" *> return Warning,
        string "error" *> return Error
      ]
  _ <- char ' '
  _ <- untilChar ':' <* " " -- Code
  msg <- untilChar '['
  _ <- untilChar ']'
  _ <- end
  return $ Message (Location (Text.cons '/' fp) c n) l [maybe Text.empty fst $ Text.unsnoc msg]
  where
    untilChar c = (takeWhile1 $ \w -> w /= c) <* char c
    end = choice [const () <$> "\n", endOfInput, return ()]
