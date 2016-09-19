module Main where

import Codec.Sarsi.Rust (messageParser)
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine ((<~), asParts, auto)
import Sarsi.Tools.Shell (mainShell)

main :: IO ()
main = mainShell "rust" (asParts <~ auto unpack <~ streamParser messageParser)
  where
    unpack (Right msg) = [msg]
    unpack (Left _)    = []
