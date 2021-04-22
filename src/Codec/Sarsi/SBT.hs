{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.SBT where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AttoText
import Data.Text (Text)
import qualified Data.Text as Text

data SBTEvent = CompileStart Text | TaskFinish Bool Text | Throw Message
  deriving (Show)

cleaningCursesSBT :: Parser Text
cleaningCursesSBT = choice [silent, empty, keep]
  where
    silent = "  | =>" >> untilLineBreak >> "\n" >> return Text.empty
    empty = do
      _ <- AttoText.takeWhile1 $ \w -> w == '\n'
      return Text.empty
    keep = do
      content <- (AttoText.takeWhile1 $ \w -> w /= '\n') <* end
      return $ content `Text.snoc` '\n'

eventParser :: Parser SBTEvent
eventParser = choice [compile, finish, Throw <$> messageParser]
  where
    compile = do
      txt <- string "[info] " *> choice ["Build triggered", "Compiling"] *> untilLineBreak <* end
      return $ CompileStart txt
    finish = do
      res <- status <* space
      txt <- string "Total time: " *> untilLineBreak <* end
      _ <- end
      return $ TaskFinish res txt
      where
        status = choice [string "[success]" *> return True, string "[error]" *> return False]

messageParser :: Parser Message
messageParser = do
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

