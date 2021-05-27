{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.NixSpec (spec) where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import qualified Codec.Sarsi.Nix as Nix
import Codec.Sarsi.Spec (parseFile)
import Test.Hspec

spec :: Spec
spec = it "should work" $ do
  xs <- parseFile Nix.messageParser "tests/nix-error.txt"
  let msg = Message (Location "/home/alois/development/nix-envs/test.nix" 12 3) Error ["undefined variable 'Ystdenv'"]
  shouldBe (head xs) (Right $ msg)
