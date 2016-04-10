module Main where

import Distribution.Text
import Sarsi (mkSockAddr, title)
import Sarsi.Trace (traceHS, traceSBT)
import System.IO (stdin)
import System.Environment (getArgs)

import Paths_sarsi (version)

main :: IO ()
main = getArgs >>= run where
  run ["--trace-hs"]  = traceHS stdin
  run ["--trace-sbt"] = traceSBT stdin
  run ["--version"]   = putStrLn $ concat [title, "-", display version]
  run _ = do
    sock <- mkSockAddr "."
    print sock
