{-# LANGUAGE Rank2Types #-}
module Sarsi.Consumer where

import Codec.Sarsi (Event, getEvent)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Exception (IOException, bracket, try)
import Data.Binary.Machine (processGet)
import Data.Machine ((<~), auto, asParts, runT_)
import Network.Socket (connect, socketToHandle)
import Sarsi (Broker(..), Topic(..), createSocket, getSockAddr)
import System.FSNotify (eventPath, watchDir, withManager)
import System.IO (IOMode(ReadMode), hClose)
import System.IO.Machine (IOSink, IOSource, byChunk, sourceHandle)

consumeOrWait :: Topic -> (IOSource Event -> IO (Maybe a)) -> IO a
consumeOrWait topic@(Topic (Broker bp) tp) f = do
  res <- consume topic f
  either (const $ withManager waitAndRetry) return res
    where
      waitAndRetry mng = do
        lck <- newEmptyMVar
        watchDir mng bp pred $ const $ putMVar lck ()
        takeMVar lck
        consumeOrWait topic f
      pred e = eventPath e == tp

consumeOrWait_ :: Topic -> IOSink Event  -> IO a
consumeOrWait_ topic@(Topic (Broker bp) _) sink =
    consumeOrWait topic f where f p = fmap (const Nothing) $ runT_ $ sink <~ p

consume :: Topic -> (IOSource Event -> IO (Maybe a)) -> IO (Either IOException a)
consume topic f = try $ consume' topic f

consume_ :: Topic -> IOSink Event -> IO (Either IOException ())
consume_ t sink = consume t f where f p = fmap (const Nothing) $ runT_ $ sink <~ p

consume' :: Topic -> (IOSource Event -> IO (Maybe a)) -> IO a
consume' topic f = do
  a <- bracket createHandle hClose process
  maybe (consume' topic f) return a
    where
      createHandle = do
        sock  <- createSocket
        connect sock $ getSockAddr topic
        socketToHandle sock ReadMode
      process h = f $ asParts <~ auto unpack <~ processGet getEvent <~ sourceHandle byChunk h
      unpack (Right e) = [e]
      unpack (Left _) = []
