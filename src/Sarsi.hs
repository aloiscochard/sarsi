module Sarsi where

import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (MD5)
import qualified Data.ByteString.Char8 as BSC8
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), defaultProtocol, socket)
import System.Directory (XdgDirectory (XdgCache), createDirectory, doesDirectoryExist, doesFileExist, getXdgDirectory, makeAbsolute, removeFile)
import System.FilePath ((</>))

title :: String
title = "sarsi"

newtype Broker = Broker FilePath

data Topic = Topic {broker :: Broker, topicPath :: FilePath, topicOrigin :: FilePath}

getBroker :: IO Broker
getBroker = do
  bp <- getXdgDirectory XdgCache title
  let b = Broker bp
  exists <- doesDirectoryExist bp
  if exists then return b else createDirectory bp >> return b

getTopic :: Broker -> FilePath -> IO Topic
getTopic b@(Broker bp) fp' = do
  fp <- makeAbsolute fp'
  return $ Topic b (bp </> (show $ (hash $ BSC8.pack fp :: Digest MD5))) fp

removeTopic :: Topic -> IO ()
removeTopic (Topic _ fp _) = removeFile fp

createSocket :: IO Socket
createSocket = do
  socket AF_UNIX Stream defaultProtocol

createSockAddr :: Topic -> IO SockAddr
createSockAddr t@(Topic _ path _) = do
  exists <- doesFileExist path
  if (exists) then removeFile path else return ()
  return $ getSockAddr t

getSockAddr :: Topic -> SockAddr
getSockAddr (Topic _ path _) = SockAddrUnix path
