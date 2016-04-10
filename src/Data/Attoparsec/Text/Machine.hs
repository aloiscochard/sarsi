module Data.Attoparsec.Text.Machine where

import Data.Attoparsec.Text (Parser, parse)
import Data.Text (Text)
import Data.Machine (ProcessT)

import Data.Attoparsec.Machine (processParserWith, streamParserWith)

streamParser :: Monad m => Parser a -> ProcessT m Text (Either String a)
streamParser p = streamParserWith $ parse p

processParser :: Monad m => Parser a -> ProcessT m Text (Either String (Text, a))
processParser p = processParserWith $ parse p
