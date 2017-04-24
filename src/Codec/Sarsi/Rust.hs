{-# LANGUAGE OverloadedStrings #-}
module Codec.Sarsi.Rust where

import Codec.Sarsi (Level(..), Location(..), Message(..))

import Data.Attoparsec.Text

messageParser :: Parser Message
messageParser = choice [formatA, formatB] where
    formatA    = do
      fp  <- takeWhile1 (\c -> c /= sepChar && c /= '\n' && c /= '\r') <* char sepChar
      n   <- decimal <* char sepChar
      c   <- decimal <* char sepChar
      _   <- untilSep <* char sepChar
      _   <- untilSpace *> space
      l   <- choice
        [ string "warning: " *> return Warning
        , string "error: " *> return Error ]
      txt <- untilLineBreak <* ("\n" <* end)
      return $ Message (Location fp c n) l [txt]

    formatB    = do
      l   <- choice
        [ string "warning"  *> untilSep0 *> string ": " *> return Warning
        , string "error"    *> untilSep0 *> string ": " *> return Error   ]
      txt <- untilLineBreak <* "\n"
      fp  <- many1 space *> "--> " *> untilSep <* char sepChar
      n   <- decimal <* char sepChar
      c   <- decimal <* end
      return $ Message (Location fp c n) l [txt]

    untilSep   = takeWhile1 $ \w -> w /= sepChar
    untilSep0  = Data.Attoparsec.Text.takeWhile $ \w -> w /= ':'
    untilSpace = takeWhile1 $ \w -> w /= ' '
    end = choice [const () <$> "\n", endOfInput, return ()]
    sepChar = ':'
    untilLineBreak = takeWhile1 $ \w -> w /= '\n' && w /= '\r'
