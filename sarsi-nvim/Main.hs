module Main where

import Codec.Sarsi (Event(..), Level(..), Location(..), Message(..), getEvent)
import Data.Machine ((<~), asParts, auto)
import Data.MessagePack.Object (Object(..), toObject)
import NVIM.Client (Command(..), runCommand)
import Sarsi (title)
import Sarsi.Consumer (consume)
import System.IO (BufferMode(NoBuffering), hSetBinaryMode, hSetBuffering, stdin, stdout)
import System.IO.Machine (sinkIO)

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Vector as Vector

echo :: String -> Command
echo str = VimCommand [toObject $ concat ["echo \"", str, "\""]]

echom :: String -> Command
echom str = VimCommand [toObject $ concat ["echom \"", title, ": ", str, "\""]]

setqflist :: String -> [Object] -> Command
setqflist action items = VimCallFunction (Text.pack "setqflist") [toObject items, toObject action]

-- TODO Sanitize text description by escaping special characters
mkQuickFix :: Message -> Object
mkQuickFix (Message (Location fp col ln) lvl txts) = toObject . Map.fromList $
  [ ("filename", toObject fp)
  , ("lnum", ObjectInt ln)
  , ("col", ObjectInt col)
  , ("type", toObject $ tpe lvl)
  , ("text", toObject $ Text.unlines $ Vector.toList txts) ]
    where
      tpe Error   = "E"
      tpe Warning = "W"

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  consume "." $ sinkIO publish <~ asParts <~ auto f
    where
      f (Notify msg@(Message loc lvl _))  =
        [setqflist "a" [mkQuickFix msg], echo $ concat [show loc, " ", show lvl]]
      f e@(Start _)   = [setqflist "r" [], echom $ show e]
      f e             = [echom $ show e ]
      publish cmd = do
        -- TODO Log failed command in a file (or print to stderr?).
        _ <- runCommand cmd
        return ()
