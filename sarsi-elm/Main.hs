{-# LANGUAGE Rank2Types #-}
module Main where

import Codec.ELM.Log (decodeMessage, toHandle)
import Codec.Sarsi (Event(..), Level(..), Message(..))
import Codec.Sarsi.ELM (fromELMLog)
import Data.Machine (ProcessT, (<~), asParts, auto, autoM, prepended, runT_, source)
import Data.List (foldl')
import Sarsi (getBroker, getTopic)
import Sarsi.Producer (produce)
import System.Environment (getArgs)
import System.IO (stderr)
import System.IO.Machine (byLine)
import System.Process (StdStream(..), shell, std_in, std_out)
import System.Process.Machine (callProcessMachines, mStdOut)
import System.Exit (ExitCode, exitWith)

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Sarsi as Sarsi

title :: String
title = concat [Sarsi.title, "-elm"]

-- TODO Use Wee/Tee to merge stdout/stderr (keep echoText splitted).
callShell :: String -> ProcessT IO Message a -> IO (ExitCode, [a])
callShell cmd sink = callProcessMachines byLine createProc (mStdOut pipeline)
  where
    pipeline = sink <~ asParts <~ auto toSarsi <~ echoMsgs stderr <~ auto unpack <~ decodeLog
    toSarsi msgs = List.map fromELMLog msgs
    echoMsgs h = autoM $ (\msgs -> mapM (toHandle h) msgs  >> return msgs)
    unpack  (Right msgs) = msgs
    unpack  (Left _) =  []
    decodeLog = auto $  decodeMessage
    createProc  = (shell cmd) { std_in = Inherit, std_out = CreatePipe }

producer :: String -> ProcessT IO Event Event -> IO ExitCode
producer cmd sink = do
  (ec, xs) <- callShell cmd pipeline
  let finish = createFinish xs
  putStrLn $ concat [title, ": ", show finish]
  runT_ $ sink <~ source [finish]
  return ec
    where
      pipeline = sink <~ prepended [Start $ Text.pack "elm"] <~ auto Notify
      createFinish xs = foldl' f empty xs
        where
          empty = Finish 0 0
          f (Finish e w) (Notify (Message _ Warning _)) = Finish e (w + 1)
          f (Finish e w) (Notify (Message _ Codec.Sarsi.Error _))   = Finish (e + 1) w
          f finish _ = finish

main :: IO ()
main = do
  args  <- getArgs
  b     <- getBroker
  t     <- getTopic b "."
  ec    <- produce t $ producer (concat $ (List.intersperse " " args) ++ [" --report=json"])
  exitWith ec
