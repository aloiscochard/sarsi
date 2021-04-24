module Main where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Version (showVersion)
import Paths_sarsi (version)
import qualified Rosetta as Rosetta
import Sarsi (getBroker, getSockAddr, getTopic, title)
import Sarsi.Processor (languageProcess, processAll, processAny)
import Sarsi.Tools.Pipe (pipe)
import Sarsi.Tools.Trace (traceCleanCurses, traceHS, traceRS, traceSBT, traceSBTCurses)
import System.Environment (getArgs)
import System.IO (stdin)

main :: IO ()
main = getArgs >>= run
  where
    run ["--trace", "clean-curses"] = traceCleanCurses stdin
    run ["--trace", "hs"] = traceHS stdin
    run ["--trace", "rs"] = traceRS stdin
    run ["--trace", "sbt"] = traceSBT stdin
    run ["--trace", "sbt-curses"] = traceSBTCurses stdin
    run ["--topic"] = do
      b <- getBroker
      t <- getTopic b "."
      print $ getSockAddr t
    run ["--version"] = putStrLn $ concat [title, "-", showVersion version]
    run [] = pipe "any" processAny
    run exts = case Set.fromList <$> traverse parseExt exts of
      Right languageTags -> do
        ps <- traverse fetchProcess $ Set.toList languageTags
        pipe (concat $ List.intersperse "+" (Rosetta.languageLabel <$> (Set.toList languageTags))) (processAll $ ps >>= id)
      Left err -> putStrLn $ concat [title, ": ", err]
    fetchProcess lt = case languageProcess lt of
      Just process -> return [process]
      Nothing -> do
        putStrLn $ concat [title, ": ", "unsupported language '", Rosetta.languageLabel lt, "'"]
        return []
    parseExt ext =
      case Rosetta.fromExtension ext of
        Just lt -> Right lt
        Nothing -> Left (concat ["invalid extension '", ext, "'."])
