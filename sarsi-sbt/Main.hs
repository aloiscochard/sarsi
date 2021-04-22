{-# LANGUAGE Rank2Types #-}

module Main where

import Codec.Sarsi (Event)
import Codec.Sarsi.SBT.Machine (eventProcess)
import qualified Data.List as List
import Data.Machine (ProcessT, autoM, runT_, (<~))
import qualified Data.Text.IO as TextIO
import Sarsi (getBroker, getTopic)
import qualified Sarsi as Sarsi
import Sarsi.Producer (produce)
import System.Environment (getArgs)
import System.Exit (ExitCode, exitWith)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin, stdout)
import System.IO.Machine (byChunk)
import System.Process (StdStream (..), shell, std_in, std_out)
import System.Process.Machine (ProcessMachines, callProcessMachines)

title :: String
title = concat [Sarsi.title, "-sbt"]

-- TODO Contribute to machines-process
mStdOut_ :: ProcessT IO a b -> ProcessMachines a a0 k0 -> IO ()
mStdOut_ mp (_, Just stdOut, _) = runT_ $ mp <~ stdOut
mStdOut_ _ _ = return ()

producer :: String -> ProcessT IO Event Event -> IO (ExitCode)
producer cmd sink = do
  (ec, _) <- callProcessMachines byChunk createProc (mStdOut_ pipeline)
  return ec
  where
    pipeline = sink <~ eventProcess <~ echoText stdout
    echoText h = autoM $ (\txt -> TextIO.hPutStr h txt >> return txt)
    createProc = (shell cmd) {std_in = Inherit, std_out = CreatePipe}

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  b <- getBroker
  t <- getTopic b "."
  ec <- produce t $ producer $ concat $ List.intersperse " " ("sbt" : "-Dsbt.color=always" : args)
  exitWith ec
