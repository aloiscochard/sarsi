module Codec.Sarsi.SBT.Machine where

import Codec.Sarsi.SBT
import Data.Attoparsec.Text.Machine (processParser, streamParser)
import Data.Machine (ProcessT, (<~), asParts, auto, autoM, runT_)
import Data.Text (Text)

import qualified Data.Text as Text

eventProcess :: Monad m => ProcessT m Text SBTEvent
eventProcess = asParts <~ auto unpackEvent <~ streamParser eventParser <~ preprocessing
  where
    preprocessing =
      asParts <~ auto unpackEC <~ processParser cleanEC <~ asParts <~ auto unpackLine <~ streamParser byLine
        where byLine = untilLineBreak <* end
    unpackEvent (Right e)  = [e]
    unpackEvent (Left _)  = []
    unpackEC (Left _)     = []
    unpackEC (Right (_, txt))  = [txt]
    unpackLine (Right txt)  = [txt `Text.snoc` '\n']
    unpackLine (Left _)     = []


