{-# LANGUAGE Rank2Types #-}
module Sarsi.Consumer where

import Codec.Sarsi (Event, getEvent)
import Control.Exception (bracket)
import Data.Binary.Machine (processGet)
import Data.Machine ((<~), auto, asParts, runT_)
import Network.Socket (connect, socketToHandle)
import Sarsi (Topic(..), createSocket, getSockAddr)
import System.IO (IOMode(ReadMode), hClose)
import System.IO.Machine (IOSink, IOSource, byChunk, sourceHandle)

-- TODO If unable to connect, hook a notification on broker path to wait and retry.

consume :: Topic -> (IOSource Event -> IO (Maybe a)) -> IO a
consume topic f = do
  a <- bracket createHandle hClose process
  maybe (consume topic f) return a
    where
      createHandle = do
        sock  <- createSocket
        connect sock $ getSockAddr topic
        socketToHandle sock ReadMode
      process h = f $ asParts <~ auto unpack <~ processGet getEvent <~ sourceHandle byChunk h
      unpack (Right e) = [e]
      unpack (Left _) = []

consume_ :: Topic -> IOSink Event -> IO ()
consume_ t sink = consume t f where f p = fmap (const Nothing) $ runT_ $ sink <~ p
