{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.NixSpec (spec) where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import qualified Codec.Sarsi.Nix as Nix
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine (runT, source, (<~))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Sarsi.Tools.Trace (appendLF)
import Test.Hspec

parseText :: Parser a -> [Text] -> IO [Either String a]
parseText parser txts =
  runT $ streamParser parser <~ appendLF <~ source txts

spec :: Spec
spec = it "should work" $ do
  txts <- Text.lines <$> TextIO.readFile "tests/nix-error.txt"
  xs <- parseText Nix.messageParser txts
  let msg = Message (Location "/home/alois/development/nix-envs/test.nix" 12 3) Error ["undefined variable 'Ystdenv'"]
  shouldBe (head xs) (Right $ msg)
