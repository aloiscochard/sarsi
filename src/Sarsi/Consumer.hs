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
import System.IO (IOMode(ReadMode), hClose, hWaitForInput)
import System.IO.Machine (IOSink, IOSource, byChunkOf, sourceHandle)

consumeOrWait :: Topic -> (Maybe s -> IOSource Event -> IO (Either s a)) -> IO a
consumeOrWait topic@(Topic (Broker bp) tp) f = do
  res <- consume topic f
  either (const $ withManager waitAndRetry) return res
    where
      waitAndRetry mng = do
        lck <- newEmptyMVar
        stop <- watchDir mng bp pred' $ const $ putMVar lck ()
        takeMVar lck
        stop
        consumeOrWait topic f
      pred' e = eventPath e == tp

consumeOrWait_ :: Topic -> IOSink Event  -> IO a
consumeOrWait_ topic@(Topic _ _) sink =
    consumeOrWait topic f where f _ src = fmap (const $ Left ()) $ runT_ $ sink <~ src

consume :: Topic -> (Maybe s -> IOSource Event -> IO (Either s a)) -> IO (Either IOException a)
consume topic f = try $ consume' topic f

consume' :: Topic -> (Maybe s -> IOSource Event -> IO (Either s a)) -> IO a
consume' topic f = bracket createHandle hClose (process Nothing)
  where
    createHandle = do
      sock  <- createSocket
      connect sock $ getSockAddr topic
      socketToHandle sock ReadMode
    process s h = do
      sa  <- f s $ asParts <~ auto unpack <~ processGet getEvent <~ sourceHandle (byChunkOf 1) h
      _   <- hWaitForInput h (-1)
      either (continue h) return sa
        where continue h' s' = process (Just s') h'
    unpack (Right e) = [e]
    unpack (Left _) = []
