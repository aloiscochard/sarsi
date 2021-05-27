module Sarsi.Tools.Pipe where

import Codec.Sarsi (Event (..), Message (..))
import Codec.Sarsi.Curses (cleanLine, cleaningCurses)
import Data.Attoparsec.Text.Machine (processParser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Machine (MachineT, ProcessT, asParts, auto, autoM, prepended, runT_, (<~))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Sarsi (Topic, getBroker, getTopic)
import Sarsi.Producer (produce)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.IO (stdin, stdout)
import System.IO.Machine (byLine, sourceHandle)

pipe :: String -> (Topic -> ProcessT IO Text Message) -> IO ()
pipe lbl process = do
  b <- getBroker
  t <- getTopic b "."
  pipeFrom lbl (process t) $ autoM echo <~ (sourceHandle byLine stdin)
  where
    echo xs = ByteString.hPutStrLn stdout xs >> return xs

pipeFrom :: String -> ProcessT IO Text Message -> MachineT IO k ByteString -> IO ()
pipeFrom lbl process source = do
  b <- getBroker
  t <- getTopic b "."
  produce t $ producer lbl process source
  exitWith ExitSuccess

producer :: String -> ProcessT IO Text Message -> MachineT IO k ByteString -> ProcessT IO Event Event -> IO ()
producer lbl process source sink = do
  runT_ $ pipeline <~ process <~ cleaning <~ auto decodeUtf8 <~ source
  where
    pipeline = sink <~ prepended [Start $ Text.pack lbl] <~ auto Notify

cleaning :: ProcessT IO Text Text
cleaning = asParts <~ auto unpack <~ processParser cleaningCurses <~ auto (\txt -> (cleanLine txt) `Text.snoc` '\n')
  where
    unpack (Right (_, txt)) = [txt]
    unpack (Left _) = []
