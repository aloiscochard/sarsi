module Sarsi where

import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (MD5)
import Network.Socket (Family(AF_UNIX), SockAddr(SockAddrUnix), Socket, SocketType(Stream), defaultProtocol, socket)
import System.Directory (getTemporaryDirectory, doesFileExist, makeAbsolute, removeFile)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as BSC8

title :: String
title = "sarsi"

-- TODO Windows compat: create a TCP socket.

mkSocket :: IO Socket
mkSocket = do
  socket AF_UNIX Stream defaultProtocol

mkSockAddr :: FilePath -> IO SockAddr
mkSockAddr fp = do
  path  <- getSockPath fp
  return . SockAddrUnix $ path

mkSockAddr' :: FilePath -> IO SockAddr
mkSockAddr' fp = do
  path  <- getSockPath fp
  exists <- doesFileExist path
  if (exists) then removeFile path else return ()
  mkSockAddr fp

getSockPath :: FilePath -> IO FilePath
getSockPath fp' = do
  fp    <- makeAbsolute fp'
  tmp <- getTemporaryDirectory
  return $ tmp </> concat [title, "-", show $ (hash $ BSC8.pack fp :: Digest MD5)]
