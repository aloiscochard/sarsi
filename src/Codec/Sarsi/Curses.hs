{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.Curses where

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AttoText
import Data.Text (Text)
import qualified Data.Text as Text

-- Note: this parser remove CSI codes and do a best effort
-- at removing "clear line" instructions while keeping
-- all information exposed without any mangling.
cleaningCurses :: Parser Text
cleaningCurses = choice [multiples, single, (AttoText.takeWhile $ \w -> w /= '\n') <* "\n"]
  where
    multiples = do
      before <- ln
      middle <- choice [silenceClearLines, silenceCSI]
      after <- choice [multiples, single]
      return (Text.concat [before, "\n", middle, after])
      where
        ln = do
          before <- AttoText.takeWhile (breakAt . fromEnum)
          after <- choice [lineFinish, lineContinue]
          return (Text.concat [before, after])
          where
            breakAt 10 = False -- LF
            breakAt 27 = False -- ESC
            breakAt _ = True
        lineFinish = char '\n' >> (return $ Text.pack "\n")
        lineContinue = csi >> ln
        silenceClearLines = do
          _ <- AttoText.takeWhile (not . isEsc . fromEnum)
          _ <- choice [AttoText.many1 (cl <* "\n"), AttoText.many1 cl]
          return Text.empty
    single = do
      befores <- many1 $ choice [clearLine, silenceCSI]
      str <- AttoText.many1 anyChar
      _ <- endOfInput
      return (Text.concat [Text.concat befores, Text.pack str])
      where
        clearLine = do
          _ <- AttoText.takeWhile (not . isEsc . fromEnum)
          _ <- AttoText.many1 cl
          return Text.empty
    cl = csiHeader >> string "2K" >> return ()
    silenceCSI = do
      txt <- AttoText.takeWhile (not . isEsc . fromEnum)
      _ <- AttoText.many1 csi
      return txt

-- CSI (Control Sequence Introducer) sequences
csi :: Parser Text
csi = do
  _ <- csiHeader
  param <- takeWhileInRange 0x30 0x3F
  inter <- takeWhileInRange 0x20 0x2F
  final <- satisfy ((inRange 0x40 0x7E) . fromEnum)
  return $ Text.concat [param, inter, Text.singleton final]
  where
    takeWhileInRange l u = AttoText.takeWhile (inRange l u . fromEnum)
    inRange l u i | i >= l && i <= u = True
    inRange _ _ _ = False

csiHeader :: Parser ()
csiHeader = (satisfy (isEsc . fromEnum) <* char '[') >> return ()

isEsc :: Int -> Bool
isEsc 27 = True
isEsc _ = False

