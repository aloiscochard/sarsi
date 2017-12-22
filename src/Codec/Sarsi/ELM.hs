module Codec.Sarsi.ELM where

import Codec.Sarsi
import Data.Text (pack)
import Data.Maybe (fromMaybe)

import qualified Codec.ELM.Log as Log
import qualified Data.Vector as Vector
import qualified Data.Text as Text

fromELMLog :: Log.Message-> Message
fromELMLog (Log.Message tag overview details region subRegion file level) =
  let col = Log.column . Log.start $ fromMaybe region subRegion
      row = Log.line . Log.start $ fromMaybe region subRegion
  in Message
       (Location (pack file) (fromIntegral col) (fromIntegral row))
       (fromELMLevel level)
       ([(Text.concat [pack tag, pack ": ", pack overview, pack " ", pack details])])

fromELMLevel :: String -> Level
fromELMLevel txt = if txt == "warning" then Warning else Error
