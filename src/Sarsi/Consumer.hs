{-# LANGUAGE Rank2Types #-}
module Sarsi.Consumer where

import Codec.Sarsi (Event, getEvent)
import Data.Binary.Machine (streamGet)
import Data.Machine ((<~), runT_)
import Network.Socket (Socket, accept, bind, listen, close, socketToHandle)
import Sarsi (mkSocket, mkSockAddr')
import System.IO (IOMode(ReadMode), hClose)
import System.IO.Machine (IOSink, byChunk, sourceHandle)

consume :: FilePath -> IOSink Event -> IO ()
consume fp sink = do
  sock  <- mkSocket
  addr  <- mkSockAddr' fp
  bind sock addr
  listen sock 1
  serve sock sink
  close sock

serve :: Socket -> IOSink Event -> IO ()
serve sock sink = do
  (conn, _) <- accept sock
  h     <- socketToHandle conn ReadMode
  runT_ $ sink <~ streamGet getEvent <~ sourceHandle (byChunk) h
  hClose h
  serve sock sink
