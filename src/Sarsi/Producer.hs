{-# LANGUAGE Rank2Types #-}

module Sarsi.Producer where

import Codec.Sarsi (Event (..), Level (..), Message (..), putEvent)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.Chan (dupChan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (IOException, bracket, tryJust)
import Data.Binary.Machine (processPut)
import Data.List (foldl')
import Data.Machine (ProcessT, prepended, runT_, (<~))
import Data.Machine.Process (takingJusts)
import Network.Socket (Socket, accept, bind, close, listen, socketToHandle)
import Sarsi (Topic, createSockAddr, createSocket, removeTopic, title)
import System.Console.ANSI
import System.IO (Handle, IOMode (WriteMode), hClose)
import System.IO.Machine (byChunk, sinkHandle, sinkIO, sourceIO)

finishPrint :: Int -> Int -> IO ()
finishPrint e w = do
  setSGR [SetColor Foreground Dull Blue]
  putStr $ title
  setSGR [Reset]
  putStr $ ": "
  setSGR (sgr e w)
  putStrLn $ show event
  setSGR [Reset]
  where
    sgr 0 0 = [SetColor Foreground Dull Green]
    sgr 0 _ = [SetColor Foreground Dull Yellow]
    sgr _ _ = [SetColor Foreground Vivid Red]
    event = Finish e w

finishCreate :: [Event] -> (Int, Int)
finishCreate xs = foldl' f empty xs
  where
    empty = (0, 0)
    f (e, w) (Notify (Message _ Warning _)) = (e, (w + 1))
    f (e, w) (Notify (Message _ Error _)) = ((e + 1), w)
    f finish _ = finish

produce :: Topic -> (ProcessT IO Event Event -> IO a) -> IO a
produce t f = do
  conns <- atomically $ newTQueue
  chan <- newChan
  state <- newMVar []
  server <- async $ bracket bindSock close (serve (process conns chan state))
  feeder <- async $ f (sinkIO $ feed chan state)
  a <- wait feeder
  es <- readMVar state
  let (errs, warns) = finishCreate es
  writeChan chan $ Just $ Finish errs warns
  writeChan chan $ Nothing
  finishPrint errs warns
  waitFinish conns
  cancel server
  removeTopic t
  return a
  where
    bindSock = do
      sock <- createSocket
      addr <- createSockAddr t
      bind sock addr
      listen sock 1
      return sock
    process conns chan' state h = do
      chan <- dupChan chan'
      es <- readMVar state
      conn <- async $ do
        runT_ $ sinkHandle byChunk h <~ processPut putEvent <~ (prepended $ reverse es) <~ takingJusts <~ (sourceIO $ readChan chan)
        hClose h
      atomically $ writeTQueue conns conn
      return Nothing
    feed chan state e = do
      modifyMVar_ state $ case e of
        (Start _) -> const $ return [e]
        _ -> return . (:) e
      writeChan chan $ Just e
    waitFinish conns = do
      conn <- atomically $ tryReadTQueue conns
      _ <- tryJust io $ maybe (return ()) wait conn
      return ()
      where
        io :: IOException -> Maybe ()
        io _ = Just ()

serve :: (Handle -> IO (Maybe a)) -> Socket -> IO a
serve f sock = bracket acceptHandle hClose process
  where
    acceptHandle = do
      (conn, _) <- accept sock
      h <- socketToHandle conn WriteMode
      return h
    process h = do
      a <- f h
      maybe (serve f sock) return a
