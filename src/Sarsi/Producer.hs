{-# LANGUAGE Rank2Types #-}
module Sarsi.Producer where

import Codec.Sarsi (Event(..), putEvent)
import Data.Binary.Machine (processPut)
import Data.Machine (ProcessT, (<~), auto, asParts)
import Data.Machine.Fanout (fanout)
import Network.Socket (connect, socketToHandle)
import Sarsi (createSocket, getBroker, getTopic, getSockAddr)
import System.IO (IOMode(AppendMode), hClose)
import System.IO.Machine (byChunk, sinkHandle)

produce :: FilePath -> (ProcessT IO Event Event -> IO a) -> IO a
produce fp f = do
  b     <- getBroker
  t     <- getTopic b fp
  sock  <- createSocket
  connect sock $ getSockAddr t
  h     <- socketToHandle sock AppendMode
  -- TODO use sinkPart_
  res   <- f $ asParts <~ fanout [auto (:[]), auto (const []) <~ sinkHandle byChunk h <~ processPut putEvent]
  hClose h
  return res

