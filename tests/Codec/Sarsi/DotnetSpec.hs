{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.DotnetSpec (spec) where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import qualified Codec.Sarsi.Dotnet as Dotnet
import Codec.Sarsi.Spec (parseFile)
import Test.Hspec

spec :: Spec
spec = it "should work" $ do
  xs <- parseFile Dotnet.messageParser "tests/dotnet-error.txt"
  let msg = Message (Location "/home/alois/development/foo/Core.fs" 1 7) Error ["The value or constructor 'aoeu' is not defined."]
  shouldBe (head . tail . tail . tail $ tail xs) (Right $ msg)
