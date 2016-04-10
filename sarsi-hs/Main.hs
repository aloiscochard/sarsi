{-# LANGUAGE Rank2Types #-}
module Main where

import Codec.GHC.Log (messageParser)
import Codec.Sarsi (Event(..), Level(..), Message(..))
import Codec.Sarsi.GHC (fromGHCLog)
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine (ProcessT, (<~), asParts, auto, autoM, prepended, runT_, source)
import Data.List (foldl')
import Sarsi.Producer (produce)
import System.Environment (getArgs)
import System.IO (stderr)
import System.IO.Machine (byLine)
import System.Process (StdStream(..), shell, std_in, std_err)
import System.Process.Machine (callProcessMachines, mStdErr)
import System.Exit (ExitCode, exitWith)

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Sarsi as Sarsi

title :: String
title = concat [Sarsi.title, "-hs"]

-- TODO Use Wee/Tee to merge stdout/stderr (keep echoText splitted).
callShell :: String -> ProcessT IO Message a -> IO (ExitCode, [a])
callShell cmd sink = callProcessMachines byLine createProc (mStdErr pipeline)
  where
    pipeline = sink <~ asParts <~ auto unpack <~ streamParser messageParser <~ echoText stderr <~ appendCR
    unpack (Right msg) = [fromGHCLog msg]
    unpack (Left _)    = []
    echoText h = autoM $ (\txt -> TextIO.hPutStr h txt >> return txt)
    createProc  = (shell cmd) { std_in = Inherit, std_err = CreatePipe }
    appendCR = auto $ (`Text.snoc` '\n')

producer :: String -> ProcessT IO Event Event -> IO ExitCode
producer cmd sink = do
  (ec, xs) <- callShell cmd pipeline
  let finish = createFinish xs
  putStrLn $ concat [title, ": ", show finish]
  runT_ $ sink <~ source [finish]
  return ec
    where
      pipeline = sink <~ prepended [Start $ Text.pack "haskell"] <~ auto Notify
      createFinish xs = foldl' f empty xs
        where
          empty = Finish 0 0
          f (Finish e w) (Notify (Message _ Warning _)) = Finish e (w + 1)
          f (Finish e w) (Notify (Message _ Error _))   = Finish (e + 1) w
          f finish _ = finish

main :: IO ()
main = do
  args  <- getArgs
  ec    <- produce "." $ producer (concat $ List.intersperse " " args)
  exitWith ec
