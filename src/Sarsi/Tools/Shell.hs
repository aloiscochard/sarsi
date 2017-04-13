module Sarsi.Tools.Shell where

import Codec.Sarsi (Event(..), Level(..), Message(..))
import Data.Machine (ProcessT, Is, (<~), auto, autoM, prepended, runT, runT_, source, wye, repeatedly, awaits, yield, Y(Z))
import Data.List (foldl')
import Data.Text (Text)
import Sarsi (getBroker, getTopic)
import Sarsi.Producer (produce)
import System.Environment (getArgs)
import System.IO (stderr, stdout)
import System.IO.Machine (byLine)
import System.Process (StdStream(..), shell, std_in, std_err, std_out)
import System.Process.Machine (ProcessMachines, callProcessMachines)
import System.Exit (ExitCode, exitWith)

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Sarsi as Sarsi

-- TODO Contribute to machine-process (as alt to mStdErr, mStdOut)
mStdOutputs :: ProcessT IO (Either a a) b -> ProcessMachines a a0 (Is a0) -> IO [b]
mStdOutputs mp (_, Just stdOut, Just stdErr)  = runT $ mp <~ wye stdErr stdOut slurp where
  slurp = repeatedly $ do
    res <- awaits Z
    yield res
mStdOutputs _  _                              = return []

callShell :: String -> ProcessT IO Text Message -> ProcessT IO Message a -> IO (ExitCode, [a])
callShell cmd parser sink = callProcessMachines byLine createProc (mStdOutputs pipeline)
  where
    pipeline    = sink <~ parser <~ appendCR <~ echo
    echo        = autoM $ \res -> case res of
      Left  txt -> TextIO.hPutStrLn stderr txt >> return txt
      Right txt -> TextIO.hPutStrLn stdout txt >> return txt
    createProc  = (shell cmd) { std_in = Inherit, std_err = CreatePipe, std_out = CreatePipe }
    appendCR    = auto $ (`Text.snoc` '\n')

producer :: String -> String -> ProcessT IO Text Message -> ProcessT IO Event Event -> IO ExitCode
producer lbl cmd parser sink = do
  (ec, xs) <- callShell cmd parser pipeline
  let finish = createFinish xs
  putStrLn $ concat [Sarsi.title, ": ", show finish]
  runT_ $ sink <~ source [finish]
  return ec
    where
      pipeline = sink <~ prepended [Start $ Text.pack lbl] <~ auto Notify
      createFinish xs = foldl' f empty xs
        where
          empty = Finish 0 0
          f (Finish e w) (Notify (Message _ Warning _)) = Finish e (w + 1)
          f (Finish e w) (Notify (Message _ Error _))   = Finish (e + 1) w
          f finish _ = finish

mainShell :: String -> ProcessT IO Text Message -> IO ()
mainShell lbl parser = do
  args  <- getArgs
  b     <- getBroker
  t     <- getTopic b "."
  ec    <- produce t $ producer lbl (concat $ List.intersperse " " args) parser
  exitWith ec
