{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.Spec where

import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine (ProcessT, runT, source, (<~))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Sarsi.Tools.Trace (appendLF)

parseFile :: Parser a -> FilePath -> IO [Either String a]
parseFile parser = parsingFile $ streamParser parser <~ appendLF

parsingFile :: ProcessT IO Text (Either String a) -> FilePath -> IO [Either String a]
parsingFile p fp = do
  txts <- Text.lines <$> TextIO.readFile fp
  parsingText p txts

parseText :: Parser a -> [Text] -> IO [Either String a]
parseText parser = parsingText $ streamParser parser <~ appendLF

parsingText :: ProcessT IO Text (Either String a) -> [Text] -> IO [Either String a]
parsingText p txts =
  runT $ p <~ source txts
