{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.GCCSpec (spec) where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import qualified Codec.Sarsi.GCC as GCC
import Codec.Sarsi.Spec (parseFile)
import Test.Hspec

msg = Message (Location "/tmp/helloworld/hello/hello.c" 1 3) Error ["unknown type name ‘oint’; did you mean ‘int’?"]

spec :: Spec
spec = it "should work" $ do
  xs <- parseFile GCC.messageParser "tests/gcc-error.txt"
  shouldBe (head xs) (Right $ msg)
