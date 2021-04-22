module NVIM.Client where

import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue, writeTBQueue)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Concurrent.STM.TVar (modifyTVar', newTVar, stateTVar)
import Data.Binary.Machine (DecodingError (..), processPut, streamGet)
import Data.Binary.Put (runPut)
import Data.ByteString (hGetSome, hPut)
import Data.ByteString.Lazy (toStrict)
import Data.Machine (autoM, final, run, runT_, source, (<~))
import qualified Data.Map as Map
import Data.MessagePack (Object (..))
import Data.MessagePack.RPC (Answer (..), Message (..), getMessage, putRequest)
import Data.Text (Text)
import qualified Data.Text as Text
import NVIM.Command (Command (..), mkRequest)
import System.IO (Handle, stdin, stdout)
import System.IO.Machine (byChunk, sinkHandle, sourceHandle, sourceIO)

-- TODO Extract a generic asynchronous RPC client
-- need to write a proper scheduler first to support timeout on transaction
-- type JobQueue a = TBQueue (IO a, Maybe a -> STM (), UTCTime)
-- type CommandQueue = TBQueue (Command, Maybe Answer -> STM (), Duration)

-- Asynchronous client
type CommandQueue = TBQueue (Command, Answer -> STM ())

ask :: CommandQueue -> Command -> IO (Async Answer)
ask q cmd = do
  answer <- newEmptyTMVarIO
  atomically $ writeTBQueue q (cmd, putTMVar answer)
  async $ atomically $ takeTMVar answer

ask' :: CommandQueue -> Command -> IO Answer
ask' q c = wait =<< ask q c

send :: CommandQueue -> Command -> IO ()
send q c = atomically $ writeTBQueue q (c, \_ -> return ())

mkConnection :: Handle -> Handle -> CommandQueue -> TBQueue (Text, [Object]) -> (DecodingError -> IO ()) -> IO (IO ())
mkConnection hIn hOut qCommands qNotifs errHandler = do
  nonce <- atomically $ newTVar 0
  runnings <- atomically $ newTVar Map.empty
  sender <- async . runT_ $ outbound nonce qCommands runnings
  receiver <- async . runT_ $ inbound runnings
  return $ wait sender >> wait receiver
  where
    outbound nonce q rs =
      sinkHandle byChunk hOut <~ processPut putRequest
        <~ autoM (publish rs nonce)
        <~ (sourceIO . atomically $ readTBQueue q)
    inbound rs = autoM (dispatch rs) <~ streamGet getMessage <~ sourceHandle byChunk hIn
    dispatch rs (Right (Response msgId a)) = do
      cb <- atomically $ stateTVar rs (\rs' -> (Map.lookup msgId rs', Map.delete msgId rs'))
      case cb of
        Just cb' -> atomically $ cb' a
        Nothing -> return ()
    dispatch _ (Right ((Notification m ps))) = do
      atomically $ writeTBQueue qNotifs (m, ps)
    dispatch _ (Left err) = errHandler err
    publish rs nonce (cmd, callback) = do
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
  let ys = run $ final <~ streamGet getMessage <~ source [xs]
  return $ mkAnswer $ ys
  where
    mkAnswer [(Right (Response _ a))] = a
    mkAnswer [(Left (DecodingError _ err))] = Error . ObjectStr $ Text.pack err
    mkAnswer _ = Error $ ObjectStr $ Text.pack "No RPC answer received."
