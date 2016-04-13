module Main where

import Codec.Sarsi (Event(..), Level(..), Location(..), Message(..))
import Data.Machine ((<~), asParts, auto, scan)
import Data.MessagePack.Object (Object(..), toObject)
import NVIM.Client (Command(..), runCommand)
import Sarsi (getBroker, getTopic, title)
import Sarsi.Consumer (consumeOrWait_)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin, stdout)
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

convert :: Bool -> Event -> (Bool, [Command])
convert _ e@(Start _)     = (True, [echom $ show e])
convert _ e@(Finish _ _)  = (True, [echom $ show e])
convert first (Notify msg@(Message loc lvl _))  = (False, xs) where
  xs =
    [ setqflist (if first then "r" else "a") [mkQuickFix msg]
    , echo $ concat [show loc, " ", show lvl] ]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  b     <- getBroker
  t     <- getTopic b "."
  consumeOrWait_ t $ sinkIO publish <~ asParts <~ auto unpack <~ scan f (True, [])
    where
      f (first, _) event = convert first event
      unpack (_, xs) = xs
      publish cmd = do
        -- TODO Log failed command in a file (or print to stderr?).
        _ <- runCommand cmd
        return ()
