{-# LANGUAGE OverloadedStrings #-}
module Codec.GHC.Log where

import Data.Attoparsec.Text
import Data.Text (Text)

data Message = Message Text Pos Level [Text]
  deriving Show

data Pos = Pos { column :: Int, line :: Int }
  deriving Show

data Level = Warning | Error
  deriving Show

messageParser :: Parser Message
messageParser = do
  fp  <- takeWhile1 (\c -> c /= sepChar && c /= '\n' && c /= '\r') <* char sepChar
  n   <- decimal <* char sepChar
  c   <- decimal <* char sepChar
  l   <- choice
    [ choice
      [ string " warning:" *> return Warning
      , string " error:" *> return Error ]
    -- Before GHC 8.0
    , choice
      [ string " Warning:" *> return Warning
      , return Error ] ]
  txt <- choice
    [ (string " [" *> untilLineBreak <* "\n") *> (multilinesComment <* end)
    , return <$> untilLineBreak <* ("\n" <* end)
    , takeLineBreak *> (multilinesComment <* end) ]
  return $ Message fp (Pos c n) l txt
  where
    multilinesComment = many1 $ ("    " *> (untilLineBreak <* "\n"))
    untilLineBreak = takeWhile1 $ \w -> w /= '\n' && w /= '\r'
    takeLineBreak = takeWhile1 $ \w -> w == '\n' || w == '\r'
    end = choice [const () <$> "\n", endOfInput, return ()]
    sepChar = ':'
