{-# LANGUAGE Rank2Types #-}

module Sarsi.Consumer where

import Codec.Sarsi (Event, getEvent)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, try)
import Data.Binary.Machine (streamGet)
import Data.Machine (asParts, auto, (<~))
import Network.Socket (connect, socketToHandle)
import Sarsi (Broker (..), Topic (..), createSocket, getSockAddr)
import System.FSNotify (eventPath, watchDir, withManager)
import System.IO (IOMode (ReadMode), hClose, hWaitForInput)
import System.IO.Machine (IOSource, byChunkOf, sourceHandle)

consumeOrWait :: Topic -> (Maybe s -> IOSource Event -> IO (Either s a)) -> IO a
consumeOrWait topic@(Topic (Broker bp) tp _) f = do
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

consume :: Topic -> (Maybe s -> IOSource Event -> IO (Either s a)) -> IO (Either IOException a)
consume topic f = try $ consume' topic f

consume' :: Topic -> (Maybe s -> IOSource Event -> IO (Either s a)) -> IO a
consume' topic f = bracket createHandle hClose (process Nothing)
  where
    createHandle = do
      sock <- createSocket
      connect sock $ getSockAddr topic
      socketToHandle sock ReadMode
    process s h = do
      sa <- f s $ asParts <~ auto unpack <~ streamGet getEvent <~ sourceHandle (byChunkOf 1) h
      _ <- hWaitForInput h (-1)
      either (continue h) return sa
      where
        continue h' s' = process (Just s') h'
    unpack (Right e) = [e]
    unpack (Left _) = []
