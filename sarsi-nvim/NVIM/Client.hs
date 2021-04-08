module NVIM.Client where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Concurrent.STM.TVar (modifyTVar', newTVar, stateTVar)
import Data.Binary.Machine (processPut, streamGet)
import Data.Binary.Put (runPut)
import Data.ByteString (hGetSome, hPut)
import Data.ByteString.Lazy (toStrict)
import Data.Machine (asParts, auto, autoM, final, run, source, (<~), runT_)
import System.IO.Machine (byChunk, sinkHandle, sourceHandle, sourceIO)
import Data.MessagePack (Object (..))
import Data.MessagePack.RPC (Answer (..), Message (..), Request (..), getMessage, putRequest)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import System.IO (Handle, stdin, stdout)

-- TODO Extract as generic RPC client?
-- need to write a proper scheduler first to support timeout on transaction
-- type JobQueue a = TBQueue (IO a, Maybe a -> STM (), UTCTime)
-- type CommandQueue = TBQueue (Command, Maybe Answer -> STM (), Duration)

data Command = VimCommand [Object] | VimCallFunction Text [Object]

type CommandQueue = TBQueue (Command, Answer -> STM ())

-- Asynchronous connection
mkConnection :: Handle -> Handle -> TBQueue (Text, [Object]) -> IO (IO (), CommandQueue)
mkConnection hIn hOut qNotifs = do
  nonce <- atomically $ newTVar 0
  runnings <- atomically $ newTVar Map.empty
  qCommands <- atomically $ newTBQueue 8
  sender <- async . runT_ $ outbound nonce qCommands runnings
  receiver <- async . runT_ $ inbound runnings
  return (wait sender >> wait receiver, qCommands)
  where
    outbound nonce q rs =
      sinkHandle byChunk hOut <~ processPut putRequest
        <~ autoM (send rs nonce)
        <~ (sourceIO . atomically $ readTBQueue q)
    inbound rs = autoM (dispatch rs) <~ asParts <~ auto unpack <~ streamGet getMessage <~ sourceHandle byChunk hIn
      where
      unpack (Left _) = []
      unpack (Right a) = [a]
    dispatch rs (Response msgId a) = do
      cb <- atomically $ stateTVar rs (\rs' -> (Map.lookup msgId rs', Map.delete msgId rs'))
      case cb of
        Just cb' -> atomically $ cb' a
        Nothing -> return ()
    dispatch _ (Notification m ps) = do
      atomically $ writeTBQueue qNotifs (m, ps)
    send rs nonce (cmd, callback) = do
      msgId <- atomically $ stateTVar nonce (\n -> (n, n + 1))
      atomically $ modifyTVar' rs (\rs' -> Map.insert msgId callback rs')
      return $ mkRequest msgId cmd

-- Helpers to run a command synchronously
runCommand :: Command -> IO Answer
runCommand = runCommandWith stdin stdout

runCommandWith :: Handle -> Handle -> Command -> IO Answer
runCommandWith hIn hOut cmd = do
  hPut hOut $ toStrict $ runPut $ putRequest $ mkRequest 0 cmd
  xs <- hGetSome hIn 1024
  let ys = run $ final <~ asParts <~ auto unpack <~ streamGet getMessage <~ source [xs]
  return $ mkAnswer $ ys
  where
    mkAnswer [a] = a
    mkAnswer _ = Error $ ObjectStr $ Text.pack "No RPC answer received."
    unpack (Right (Response _ a)) = [a]
    unpack _ = []

-- Internal
mkRequest :: Int -> Command -> Request
mkRequest msgId (VimCommand xs) =
  Request msgId (Text.pack "nvim_command") xs
mkRequest msgId (VimCallFunction m xs) =
  Request msgId (Text.pack "nvim_call_function") [ObjectStr m, ObjectArray (Vector.fromList xs)]
