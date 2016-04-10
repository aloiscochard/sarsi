module Sarsi.Trace where

import Codec.GHC.Log (messageParser)
import Data.Machine ((<~), auto, runT_)
import Data.Attoparsec.Text.Machine (streamParser)
import System.IO (Handle)
import System.IO.Machine (byLine, printer, sourceHandle)

import qualified Data.Text as Text

traceHS :: Handle -> IO ()
traceHS h = do
  runT_ $ printer <~ streamParser messageParser <~ appendCR <~ sourceHandle byLine h
    where
      appendCR = auto $ (`Text.snoc` '\n')
