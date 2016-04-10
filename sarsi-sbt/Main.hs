module Main where

import Codec.Sarsi (Event)
import Codec.Sarsi.SBT.Machine (eventProcess)
import Data.Machine (ProcessT, (<~), asParts, auto, autoM, runT_)
import Sarsi.Producer (produce)
import System.Environment (getArgs)
import System.Exit (ExitCode, exitWith)
import System.Process (CreateProcess, StdStream(..), shell, std_in, std_out, std_err)
import System.Process.Machine (callProcessMachines, mStdOut)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin, stdout)
import System.IO.Machine (byChunk, printer)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Sarsi as Sarsi

title :: String
title = concat [Sarsi.title, "-sbt"]

callShell :: String -> ProcessT IO Event a -> IO (ExitCode, [a])
callShell cmd sink = callProcessMachines byChunk createProc (mStdOut pipeline)
  where
    pipeline = sink <~ eventProcess <~ echoText stdout
    echoText h = autoM $ (\txt -> TextIO.hPutStr h txt >> return txt)
    createProc  = (shell cmd) { std_in = Inherit, std_out = CreatePipe }

producer :: String -> ProcessT IO Event Event -> IO ExitCode
producer cmd sink = do
  (ec, _)   <- callShell cmd sink
  return ec

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  args  <- getArgs
  ec    <- produce "." $ producer $ concat $ List.intersperse " " ("sbt":args)
  exitWith ec
