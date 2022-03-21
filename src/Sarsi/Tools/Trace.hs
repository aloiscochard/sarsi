module Sarsi.Tools.Trace where

import qualified Codec.GHC.Log as GHC
import Codec.Sarsi.Curses (cleanLine, cleaningCurses)
import qualified Codec.Sarsi.GCC as GCC
import qualified Codec.Sarsi.Rust as Rust
import qualified Codec.Sarsi.Scala as Scala
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine (ProcessT, asParts, auto, autoM, runT_, (<~))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (Handle)
import System.IO.Machine (byLine, printer, sourceHandle)

traceCleanCurses :: Handle -> IO ()
traceCleanCurses h = runT_ $ autoM printing <~ preprocessing <~ appendLF <~ sourceHandle byLine h
  where
    printing (Right s) = TextIO.putStrLn s
    printing e = putStrLn $ show e
    preprocessing = streamParser cleaningCurses <~ asParts <~ auto unpackLine <~ asLines
      where
        asLines = streamParser $ Scala.untilLineBreak <* Scala.end
    unpackLine (Right txt) = [(cleanLine txt) `Text.snoc` '\n']
    unpackLine (Left _) = []

traceGCC :: Handle -> IO ()
traceGCC = traceParser GCC.messageParser

traceHS :: Handle -> IO ()
traceHS = traceParser GHC.messageParser

traceRS :: Handle -> IO ()
traceRS = traceParser Rust.messageParser

traceParser :: Show a => Parser a -> Handle -> IO ()
traceParser parser h = do
  runT_ $ printer <~ streamParser parser <~ appendLF <~ sourceHandle byLine h

appendLF :: ProcessT IO Text Text
appendLF = auto $ (`Text.snoc` '\n')
