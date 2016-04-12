{-# LANGUAGE Rank2Types #-}
module Sarsi.Consumer where

import Codec.Sarsi (Event, getEvent)
import Data.Binary.Machine (streamGet)
import Data.Machine (ProcessT, (<~), asParts, auto, runT, runT_)
import Network.Socket (Socket, accept, bind, listen, close, socketToHandle)
import Sarsi (createSocket, createSockAddr, getBroker, getTopic)
import System.IO (Handle, IOMode(ReadMode), hClose)
import System.IO.Machine (IOSink, IOSource, byChunk, sourceHandle)

-- TODO Use bracket to close properly in case of error

consume :: FilePath -> ProcessT IO Event a -> IO [a]
consume fp sink = consume' fp f where f p = fmap Just $ runT $ sink <~ p

consume_ :: FilePath -> IOSink Event -> IO ()
consume_ fp sink = consume' fp f where f p = fmap (const Nothing) $ runT_ $ sink <~ p

consume' :: FilePath -> (IOSource Event -> IO (Maybe b)) -> IO b
consume' fp f = consumeWith fp g
  where
    g h = f $ asParts <~ auto unpack <~ streamGet getEvent <~ sourceHandle (byChunk) h
    unpack (Right e) = [e]
    unpack (Left _) = []

consumeWith :: FilePath -> (Handle -> IO (Maybe a)) -> IO a
consumeWith fp f = do
  b     <- getBroker
  t     <- getTopic b fp
  sock  <- createSocket
  addr  <- createSockAddr t
  bind sock addr
  listen sock 1
  a <- serve sock f
  close sock
  return a

serve :: Socket -> (Handle -> IO (Maybe a)) -> IO a
serve sock f = do
  (conn, _) <- accept sock
  h         <- socketToHandle conn ReadMode
  a         <- f h
  hClose h
  maybe (serve sock f) return a
