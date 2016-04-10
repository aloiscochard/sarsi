module Main where

import Sarsi (mkSockAddr)
import Sarsi.Trace (traceHS, traceSBT)
import System.IO (stdin)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run where
  run ["--trace-hs"]  = traceHS stdin
  run ["--trace-sbt"] = traceSBT stdin
  run [] = do
    sock <- mkSockAddr "."
    print sock
