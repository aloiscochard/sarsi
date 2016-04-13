{-# LANGUAGE Rank2Types #-}
module Sarsi.Producer where

import Codec.Sarsi (Event, putEvent)
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.Chan (dupChan, newChan, readChan, writeChan)
import Data.Binary.Machine (processPut)
import Data.Machine ((<~), runT_)
import Network.Socket (Socket, accept, bind, close, connect, listen, socketToHandle)
import Sarsi (Topic, createSocket, createSockAddr, removeTopic)
import System.IO (IOMode(WriteMode), Handle, hClose)
import System.IO.Machine (IOSink, byChunk, sinkIO, sinkHandle, sourceIO)

produce :: Topic -> (IOSink Event -> IO a) -> IO a
produce t f = do
  chan    <- newChan
  feeder  <- async $ f $ sinkIO $ writeChan chan
  server  <- async $ bracket bindSock close (serve (process chan))
  a       <- wait feeder
  cancel server
  removeTopic t
  return a
    where
      bindSock = do
        sock  <- createSocket
        addr  <- createSockAddr t
        bind sock addr
        listen sock 1
        return sock
      process chan' h = do
        chan <- dupChan chan'
        _    <- forkIO . runT_ $ sinkHandle byChunk h <~ processPut putEvent <~ (sourceIO $ readChan chan)
        return Nothing

serve :: (Handle -> IO (Maybe a)) -> Socket -> IO a
serve f sock = bracket acceptHandle hClose process
  where
    acceptHandle = do
      (conn, _) <- accept sock
      h         <- socketToHandle conn WriteMode
      return h
    process h = do
      a         <- f h
      maybe (serve f sock) return a
