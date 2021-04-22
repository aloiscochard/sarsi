{-# LANGUAGE OverloadedStrings #-}

module Data.Attoparsec.Text.Machine where

import Data.Attoparsec.Machine (processParserWith, streamParserWith)
import Data.Attoparsec.Text (Parser, parse, takeWhile)
import Data.Machine (ProcessT, asParts, auto, (<~))
import Data.Text (Text)

asLines :: Monad m => ProcessT m Text Text
asLines = asParts <~ auto unpackLine <~ streamParser ((Data.Attoparsec.Text.takeWhile $ \w -> w /= '\n') <* "\n")
  where
    unpackLine (Right txt) = [txt]
    unpackLine (Left _) = []

processParser :: Monad m => Parser a -> ProcessT m Text (Either String (Text, a))
processParser p = processParserWith $ parse p

streamParser :: Monad m => Parser a -> ProcessT m Text (Either String a)
streamParser p = streamParserWith $ parse p
