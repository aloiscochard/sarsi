{-# LANGUAGE OverloadedStrings #-}
module Codec.Sarsi.SBT where

import Codec.Sarsi (Level(..), Location(..), Message(..))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Text (Text)

import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Text as Text
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
  ts  <- manyTill' (lineStart *> (untilLineBreak <* "\n")) (lookAhead $ column')
  col <- column'
  _   <- end
  return $ Message (Location fp (col) ln) lvl $ formatTxts t ts
    where
      level = choice [string "[error]" *> return Error, string "[warn]" *> return Warning]
      lineStart = level <* space
      sepChar = ':'
      formatTxts t [] = Vector.singleton t
      formatTxts t ts = Vector.fromList $ t : init ts
      column' = level *> ((length <$> many1 space) <* "^\n")

cleanEC :: Parser Text
cleanEC = choice [noEC, withEC]
  where
    noEC    = takeStart <* endOfInput
    withEC  = do
      before <- takeStart <* anyChar
      _      <- takeEnd   <* anyChar
      after  <- cleanEC
      return (Text.concat [before, after])
    takeStart = AttoText.takeWhile (not . isEscStart . fromEnum)
    takeEnd   = AttoText.takeWhile (not . isEscEnd . fromEnum)
    isEscStart 27 = True
    isEscStart _  = False
    isEscEnd 109 = True
    isEscEnd _ = False

untilLineBreak :: Parser Text
untilLineBreak = takeWhile1 $ \w -> w /= '\n' && w /= '\r'

end :: Parser ()
end = choice [const () <$> "\n", endOfInput, return ()]

