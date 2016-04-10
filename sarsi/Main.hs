module Main where

import Sarsi (mkSockAddr)
import Sarsi.Trace (traceHS)
import System.IO (stdin)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run where
  run ["--trace-hs"] = traceHS stdin
  run [] = do
    sock <- mkSockAddr "."
    print sock
