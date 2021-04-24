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
import System.Process

main :: IO ()
main = do
  args <- getArgs
  (dispatchErrRead, dispatchErrWrite) <- createPipe
  (dispatchOutRead, dispatchOutWrite) <- createPipe
  (Nothing, Just hout, Just herr, hprocess) <- createProcess $ cp (concat $ List.intersperse " " args)
  conveyorErr <- conveyorRun stderr herr dispatchErrWrite
  conveyorOut <- conveyorRun stdout hout dispatchOutWrite
  let sourceErr = sourceHandle byLine dispatchErrRead
  let sourceOut = sourceHandle byLine dispatchOutRead
  let process = processAny
  worker <- async $ pipeFrom "any" process $ (auto merge) <~ wye sourceErr sourceOut slurp
  wait conveyorErr
  wait conveyorOut
  wait worker
  ec <- waitForProcess hprocess
  exitWith ec
  where
    cp cmd =
      (proc "script" ["-qec", cmd, "/dev/null"])
        { delegate_ctlc = True,
          std_in = Inherit,
          std_out = CreatePipe,
          std_err = CreatePipe
        }
    conveyorRun hOutput hInput hDispatchWrite =
      async . runT_ $
        (sinkHandleWith ByteString.hPut hOutput) <~ autoM (\xs -> ByteString.hPut hDispatchWrite xs >> return xs)
          <~ (sourceHandleWith (\h -> ByteString.hGetNonBlocking h 1) hInput)
    merge (Right x) = x
    merge (Left x) = x
    slurp = repeatedly $ do
      res <- awaits Z
      yield res
