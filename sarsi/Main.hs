module Main where

import Distribution.Text
import Sarsi (getBroker, getSockAddr, getTopic, title)
import Sarsi.Trace (traceHS, traceRS, traceSBT)
import System.IO (stdin)
import System.Environment (getArgs)

import Paths_sarsi (version)

main :: IO ()
main = getArgs >>= run where
  run ["--trace-hs"]  = traceHS stdin
  run ["--trace-rs"]  = traceRS stdin
  run ["--trace-sbt"] = traceSBT stdin
  run ["--version"]   = putStrLn $ concat [title, "-", display version]
  run _ = do
    b     <- getBroker
    t     <- getTopic b "."
    print $ getSockAddr t
