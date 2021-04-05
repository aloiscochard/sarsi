module Main where

import Control.Concurrent.Async (async, wait)
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import Data.Machine (Y (Z), auto, autoM, awaits, repeatedly, runT_, wye, yield, (<~))
import Sarsi.Processor (processAny)
import Sarsi.Tools.Pipe (pipeFrom)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.IO (stderr, stdout)
import System.IO.Machine (byLine, sinkHandleWith, sourceHandle, sourceHandleWith)
import System.Process (StdStream (..), createPipe, createProcess, shell, std_err, std_in, std_out, waitForProcess)

main :: IO ()
main = do
  args <- getArgs
  let cp = shell $ (concat $ List.intersperse " " args)
  -- let projectTag = findProjecTag >>= headOption args
  (dispatchErrRead, dispatchErrWrite) <- createPipe
  (dispatchOutRead, dispatchOutWrite) <- createPipe
  (Nothing, Just hout, Just herr, hprocess) <-
    createProcess $ cp {std_in = Inherit, std_out = CreatePipe, std_err = CreatePipe}
  conveyorErr <- conveyorRun stderr herr dispatchErrWrite
  conveyorOut <- conveyorRun stdout hout dispatchOutWrite
  let sourceErr = sourceHandle byLine dispatchErrRead
  let sourceOut = sourceHandle byLine dispatchOutRead
  let process = processAny
  worker <-
    async $ pipeFrom "any" process $ (auto merge) <~ wye sourceErr sourceOut slurp
  -- async . runT_ $ (auto ByteString.length) <~ (auto merge) <~ wye sourceErr sourceOut slurp
  wait conveyorErr
  wait conveyorOut
  wait worker
  ec <- waitForProcess hprocess
  exitWith ec
  where
    conveyorRun hOutput hInput hDispatchWrite =
      async . runT_ $
        (sinkHandleWith ByteString.hPut hOutput) <~ autoM (\xs -> ByteString.hPut hDispatchWrite xs >> return xs)
          <~ (sourceHandleWith (\h -> ByteString.hGetNonBlocking h 1) hInput)
    merge (Right x) = x
    merge (Left x) = x
    slurp = repeatedly $ do
      res <- awaits Z
      yield res
