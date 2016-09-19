{-# LANGUAGE Rank2Types #-}
module Main where

import Codec.GHC.Log (messageParser)
import Codec.Sarsi.GHC (fromGHCLog)
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine ((<~), asParts, auto)
import Sarsi.Tools.Shell (mainShell)

main :: IO ()
main = mainShell "haskell" (asParts <~ auto unpack <~ streamParser messageParser)
  where
    unpack (Right msg) = [fromGHCLog msg]
    unpack (Left _)    = []
