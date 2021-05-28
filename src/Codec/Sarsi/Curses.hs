{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.Curses where

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AttoText
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- note: expect a line that does NOT ends with a LF
cleanLine :: Text -> Text
cleanLine txt | Text.null txt = txt
cleanLine txt | Text.last txt == '\r' = fromMaybe Text.empty $ (f . fst) <$> Text.unsnoc txt
  where
    f x = case Text.breakOnAll "\r" x of
      [] -> x
      xs -> Text.tail $ (snd . last) xs
cleanLine txt = txt

-- Note: this parser remove escape sequences and do a best effort
-- at removing "clear line" instructions while keeping
-- all information exposed without any mangling.
cleaningCurses :: Parser Text
cleaningCurses = choice [multiples, single, none]
  where
    multiples = do
      before <- ln
      middle <- choice [silenceClearLines, silenceNoise]
      after <- choice [multiples, single]
      return $ Text.concat [before, "\n", middle, after]
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
        lineContinue = noise >> ln
        silenceClearLines = do
          _ <- AttoText.takeWhile (not . isEsc . fromEnum)
          _ <- choice [AttoText.many1 (cl <* "\n"), AttoText.many1 cl]
          return Text.empty
    single = do
      befores <- many1 $ choice [clearLine, silenceNoise]
      str <- AttoText.many1 anyChar
      _ <- endOfInput
      return (Text.concat [Text.concat befores, Text.pack str])
      where
        clearLine = do
          _ <- AttoText.takeWhile (not . isEsc . fromEnum)
          _ <- AttoText.many1 cl
          return Text.empty
    none = do
      ln <- (AttoText.takeWhile $ \w -> w /= '\n')
      _ <- "\n"
      return $ Text.concat [ln, "\n"]
    cl = csiHeader >> string "2K" >> return ()
    noise = ansiEscapeSeq
    silenceNoise = do
      txt <- AttoText.takeWhile (not . isEsc . fromEnum)
      _ <- AttoText.many1 noise
      return txt

ansiEscapeSeq :: Parser Text
ansiEscapeSeq = choice [csi, Text.singleton <$> controlChar]
  where
    -- TODO "Fe Escape Sequences" support
    controlChar = escape *> satisfy ((inRange 0x20 0x7F) . fromEnum)

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

inRange :: Int -> Int -> Int -> Bool
inRange l u i | i >= l && i <= u = True
inRange _ _ _ = False

escape :: Parser ()
escape = satisfy (isEsc . fromEnum) >> return ()

csiHeader :: Parser ()
csiHeader = (escape <* char '[') >> return ()

isEsc :: Int -> Bool
isEsc 27 = True
isEsc _ = False
