{-# LANGUAGE Rank2Types #-}
module Sarsi.Producer where

import Codec.Sarsi (Event(Start), putEvent)
import Control.Exception (IOException, bracket, tryJust)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.Chan (dupChan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueue, tryReadTQueue, writeTQueue)
import Data.Binary.Machine (processPut)
import Data.Machine (ProcessT, (<~), runT_, sinkPart_, prepended)
import Network.Socket (Socket, accept, bind, close, listen, socketToHandle)
import Sarsi (Topic, createSocket, createSockAddr, removeTopic)
import System.IO (IOMode(WriteMode), Handle, hClose)
import System.IO.Machine (byChunk, sinkIO, sinkHandle, sourceIO)

produce :: Topic -> (ProcessT IO Event Event -> IO a) -> IO a
produce t f = do
  conns   <- atomically $ newTQueue
  chan    <- newChan
  state   <- newMVar []
  server  <- async $ bracket bindSock close (serve (process conns chan state))
  feeder  <- async $ f $ sinkPart_ (\x -> (x, x)) (sinkIO $ feed chan state)
  a       <- wait feeder
  waitFinish conns
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
      process conns chan' state h = do
        chan <- dupChan chan'
        es   <- readMVar state
        conn <- async $ do
          runT_ $ sinkHandle byChunk h <~ processPut putEvent <~ (prepended $ reverse es) <~ (sourceIO $ readChan chan)
          hClose h
        atomically $ writeTQueue conns conn
        return Nothing
      feed chan state e = do
        modifyMVar_ state $ case e of
          (Start _) -> const $ return [e]
          _         -> return . (:) e
        writeChan chan e
      waitFinish conns = do
        conn <- atomically $ tryReadTQueue conns
        _    <- tryJust io $ maybe (return ()) wait conn
        return ()
          where
            io :: IOException -> Maybe ()
            io _ = Just ()

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
