module Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueue, writeTBQueue)
import Codec.Sarsi (Event (..), Level (..), Location (..), Message (..))
import Data.Machine (ProcessT, asParts, final, runT, scan, sinkPart_, (<~))
import Data.MessagePack (Object (..))
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import NVIM.Client (Command (..), mkConnection)
import Sarsi (getBroker, getTopic, title)
import Sarsi.Consumer (consumeOrWait)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin, stdout)
import System.IO.Machine (sinkIO)

echo :: String -> Command
echo str = VimCommand [ObjectStr . Text.pack $ concat ["echo \"", str, "\""]]

echom :: String -> Command
echom str = VimCommand [ObjectStr . Text.pack $ concat ["echom \"", title, ": ", str, "\""]]

setqflist :: String -> [Object] -> Command
setqflist action items =
  VimCallFunction
    (Text.pack "setqflist")
    [ ObjectArray (Vector.fromList items),
      ObjectStr $ Text.pack action
    ]

setqflistEmpty :: Command
setqflistEmpty = setqflist "r" []

-- TODO Sanitize text description by escaping special characters
mkQuickFix :: Message -> Object
mkQuickFix (Message (Location fp col ln) lvl txts) =
  ObjectMap $
    ( Vector.fromList
        [ (ObjectStr $ Text.pack "filename", ObjectStr fp),
          (ObjectStr $ Text.pack "lnum", ObjectInt ln),
          (ObjectStr $ Text.pack "col", ObjectInt col),
          (ObjectStr $ Text.pack "type", ObjectStr . Text.pack $ tpe lvl),
          (ObjectStr $ Text.pack "text", ObjectStr $ Text.unlines txts)
        ]
    )
  where
    tpe Error = "E"
    tpe Warning = "W"

convert :: Int -> Event -> (Int, [Command])
convert _ e@(Start _) = (0, [echom $ show e])
convert i e@(Finish _ _) = (0, (echom $ show e) : (if i == 0 then [setqflistEmpty] else []))
convert i (Notify msg@(Message loc lvl _)) = (i + 1, xs)
  where
    xs =
      [ setqflist (if i == 0 then "r" else "a") [mkQuickFix msg],
        echo $ concat [show loc, " ", show lvl]
      ]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  b <- getBroker
  t <- getTopic b "."
  qNotifs <- atomically $ newTBQueue 8
  (wait, qCommands) <- mkConnection stdin stdout qNotifs
  _ <- consumeOrWait t (consumer qCommands)
  wait
  where
    consumer q Nothing src = consumer q (Just 0) src
    consumer q (Just i) src = do
      i' <- runT $ final <~ sinkPart_ id (sinkIO (publish q) <~ asParts) <~ converter i <~ src
      return (Left $ head i')
    converter :: Int -> ProcessT IO Event (Int, [Command])
    converter i = scan f (i, []) where f (first, _) event = convert first event
    publish q cmd = atomically $ writeTBQueue q (cmd, \_ -> return ())
