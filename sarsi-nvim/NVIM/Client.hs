module NVIM.Client where

-- | A primitive NVIM RPC synchronous client.

import Data.Binary.Machine (streamGet)
import Data.Binary.Put (runPut)
import Data.ByteString (hGetSome, hPut)
import Data.ByteString.Lazy (toStrict)
import Data.Machine ((<~), asParts, auto, final, run, source)
import Data.MessagePack (Object(..))
import Data.MessagePack.RPC (Answer(..), Request(..), Message(..), getMessage, putRequest)
import Data.Text (Text)
import System.IO (Handle, stdin, stdout)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

data Command = VimCommand [Object] | VimCallFunction Text [Object]

runCommand :: Command -> IO Answer
runCommand = runCommandWith stdin stdout

runCommandWith :: Handle -> Handle -> Command -> IO Answer
runCommandWith hIn hOut cmd = do
  hPut hOut $ toStrict $ runPut $ putRequest $ mkRequest cmd
  xs <- hGetSome hIn 1024
  let ys = run $ final <~ asParts <~ auto unpack <~ streamGet getMessage <~ source [xs]
  return $ mkAnswer $ ys
    where
      mkAnswer [a] = a
      mkAnswer _   = Error $ ObjectStr $ Text.pack "No RPC answer received."
      mkRequest (VimCommand xs) =
        Request 0 (Text.pack "nvim_command") xs
      mkRequest (VimCallFunction m xs) =
        Request 0 (Text.pack "nvim_call_function") [ ObjectStr m, ObjectArray (Vector.fromList xs) ]
      unpack (Right (Response _ a)) = [a]
      unpack _                      = []
