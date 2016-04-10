{-# LANGUAGE Rank2Types #-}
module Sarsi.Producer where

import Codec.Sarsi (Event(..), putEvent)
import Data.Binary.Machine (processPut)
import Data.Machine (ProcessT, (<~), auto, asParts, runT_)
import Data.Machine.Fanout (fanout)
import Network.Socket (connect, socketToHandle)
import System.IO (IOMode(AppendMode), hClose)
import System.IO.Machine (byChunk, sinkHandle)

import qualified Data.Text as T

import Sarsi (mkSocket, mkSockAddr)

produce :: FilePath -> (ProcessT IO Event Event -> IO a) -> IO a
produce fp f = do
  sock  <- mkSocket
  addr  <- mkSockAddr fp
  connect sock addr
  h     <- socketToHandle sock AppendMode
  -- TODO use sinkPart_
  res   <- f $ asParts <~ fanout [auto (:[]), auto (const []) <~ sinkHandle byChunk h <~ processPut putEvent]
  hClose h
  return res

