module Codec.Sarsi.GHC where

import Codec.Sarsi
import Data.Text (strip)

import qualified Codec.GHC.Log as Log

fromGHCLog :: Log.Message -> Message
fromGHCLog (Log.Message fp (Log.Pos col ln) lvl texts) =
  Message
    (Location (strip fp) (fromIntegral col) (fromIntegral ln))
    (fromGHCLevel lvl) (strip <$> texts)

fromGHCLevel :: Log.Level -> Level
fromGHCLevel Log.Warning = Warning
fromGHCLevel Log.Error = Error

