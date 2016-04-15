module Sarsi where

import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (MD5)
import Network.Socket (Family(AF_UNIX), SockAddr(SockAddrUnix), Socket, SocketType(Stream), defaultProtocol, socket)
import System.Directory (getTemporaryDirectory, createDirectory, doesDirectoryExist, doesFileExist, makeAbsolute, removeFile)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as BSC8

title :: String
title = "sarsi"

newtype Broker = Broker FilePath

data Topic = Topic Broker FilePath

getBroker :: IO Broker
getBroker = do
  tmp <- getTemporaryDirectory
  let bp = tmp </> title
  let broker = Broker bp
  exists <- doesDirectoryExist bp
  if exists then return broker else createDirectory bp >> return broker

getTopic :: Broker -> FilePath -> IO Topic
getTopic b@(Broker bp) fp' = do
  fp    <- makeAbsolute fp'
  return $ Topic b $ bp </> (show $ (hash $ BSC8.pack fp :: Digest MD5))

removeTopic :: Topic -> IO ()
removeTopic (Topic _ fp) = removeFile fp

createSocket :: IO Socket
createSocket = do
  socket AF_UNIX Stream defaultProtocol

createSockAddr :: Topic -> IO SockAddr
createSockAddr t@(Topic _ path) = do
  exists <- doesFileExist path
  if (exists) then removeFile path else return ()
  return $ getSockAddr t

getSockAddr :: Topic -> SockAddr
getSockAddr (Topic _ path) = SockAddrUnix path
