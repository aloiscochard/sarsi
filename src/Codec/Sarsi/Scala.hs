{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.Scala where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AttoText
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (makeRelative)

messageParser :: FilePath -> Parser Message
messageParser root = f <$> messageParserAbsolute
  where
    f (Message loc lvl txts) =
      (Message (loc {filePath = Text.pack $ makeRelative root (Text.unpack $ filePath loc)}) lvl txts)

messageParserAbsolute :: Parser Message
messageParserAbsolute = do
  lvl <- lineStart
  fp <- takeWhile1 (\c -> c /= sepChar && c /= '\n') <* char sepChar
  ln <- decimal <* char sepChar
  col <- decimal <* char sepChar
  t <- space *> (untilLineBreak <* "\n")
  ts <- manyTill' (lineStart *> (untilLineBreak <* "\n")) (lookAhead $ column')
  _ <- column' -- ignored as it was parsed above
  _ <- end
  return $ Message (Location fp col ln) lvl $ formatTxts t ts
  where
    level = choice [string "[error]" *> return Error, string "[warn]" *> return Warning]
    lineStart = level <* space
    sepChar = ':'
    formatTxts t [] = [t]
    formatTxts t ts = t : init ts
    column' = level *> ((length <$> many1 space) <* "^\n")

untilLineBreak :: Parser Text
untilLineBreak = takeWhile1 $ \w -> w /= '\n'

end :: Parser ()
end = choice [const () <$> "\n", endOfInput, return ()]
