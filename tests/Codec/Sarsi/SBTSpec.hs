{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.SBTSpec (spec) where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import qualified Codec.Sarsi.SBT.Machine as SBT
import qualified Codec.Sarsi.Scala as Scala
import Codec.Sarsi.Spec (parsingFile)
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine ((<~))
import Sarsi.Tools.Pipe (cleaning)
import Test.Hspec

spec :: Spec
spec = it "should work" $ do
  let processor = (streamParser $ Scala.messageParser "/home/thomas/dev/explorer-backend/" {-- <~ (auto traceShowId) --}) <~ SBT.cleaningCursesSBT <~ cleaning
  xs <- parsingFile processor "tests/sbt-error.txt"
  _ <- traverse (putStrLn . show) xs
  let msg = Message (Location "src/main/scala/org/alephium/explorer/web/TransactionServer.scala" 1 27) Error ["expected class or object definition"]
  shouldBe (head $ tail $ tail xs) (Right $ msg)
