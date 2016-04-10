module NVIM.Info where

import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Binary.Get (runGet)
import Data.ByteString.Lazy (fromChunks)
import Data.Machine (echo)
import Data.Text.Lazy.Builder (toLazyText)
import System.IO.Machine (byChunkOf, printer)
import System.Process (StdStream(CreatePipe), readCreateProcessWithExitCode, shell, std_out)
import System.Process.Machine (callProcessMachines, mStdOut)

import Data.MessagePack.Object (Object)
import Data.MessagePack.ToJSON (toJSON)

import qualified Data.ByteString as BS
import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy.Char8 as BSC8

printAPI :: IO ()
printAPI = do
  (ec, chunks) <- callProcessMachines (byChunkOf 256) cp (mStdOut echo)
  f $ MP.unpack $ fromChunks chunks
  return ()
    where
     cp = (shell "nvim --api-info") { std_out = CreatePipe }
     f :: Maybe Object -> IO ()
     f (Just obj) = print $ toLazyText $ encodeToTextBuilder $ toJSON obj
     f Nothing = return ()
