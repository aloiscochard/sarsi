module Main where

import Data.Version (showVersion)
import Paths_sarsi (version)
import Sarsi (getBroker, getSockAddr, getTopic, title)
import Sarsi.Tools.Trace (traceHS, traceRS, traceSBT)
import System.Environment (getArgs)
import System.IO (stdin)

main :: IO ()
main = getArgs >>= run
  where
    run ["--trace-hs"] = traceHS stdin
    run ["--trace-rs"] = traceRS stdin
    run ["--trace-sbt"] = traceSBT stdin
    run ["--version"] = putStrLn $ concat [title, "-", showVersion version]
    run _ = do
      b <- getBroker
      t <- getTopic b "."
      print $ getSockAddr t
