{-# LANGUAGE OverloadedStrings #-}
module Codec.Sarsi.SBT where

import Codec.Sarsi (Level(..), Location(..), Message(..))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Text (Text)

import qualified Data.Vector as Vector

data SBTEvent = CompileStart Text | TaskFinish Bool Text | Throw Message
  deriving Show

eventParser :: Parser SBTEvent
eventParser = choice [compile, finish, Throw <$> messageParser]
  where
    compile = do
      txt <- string "[info] Compiling " *> untilLineBreak <* end
      return $ CompileStart txt
    finish = do
      res <- status <* space
      txt <- string "Total time: " *> untilLineBreak <* end
      _   <- end
      return $ TaskFinish res txt
        where
          status = choice [string "[success]" *> return True, string "[error]" *> return False]

messageParser :: Parser Message
messageParser = do
  lvl <- lineStart
  fp  <- takeWhile1 (\c -> c /= sepChar && c /= '\n' && c /= '\r') <* char sepChar
  ln  <- decimal <* char sepChar
  t   <- space *> (untilLineBreak <* "\n")
  ts  <- manyTill' (lineStart *> (untilLineBreak <* "\n")) (lookAhead $ column)
  col <- column
  _   <- end
  return $ Message (Location fp (col + 1) ln) lvl $ formatTxts t ts
    where
      takeLineBreak = takeWhile1 $ \w -> w == '\n' || w == '\r'
      level = choice [string "[error]" *> return Error, string "[warn]" *> return Warning]
      lineStart = level <* space
      sepChar = ':'
      formatTxts t [] = Vector.singleton t
      formatTxts t ts = Vector.fromList $ t : init ts
      column = lineStart *> ((length <$> many1 space) <* "^\n")

untilLineBreak = takeWhile1 $ \w -> w /= '\n' && w /= '\r'
end = choice [const () <$> "\n", endOfInput, return ()]
