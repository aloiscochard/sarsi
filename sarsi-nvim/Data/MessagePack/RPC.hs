module Data.MessagePack.RPC where

import Data.Binary (Get, Put, get, getWord8, put)
import Data.MessagePack (Object(..))
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.MessagePack as MP
import qualified Data.Vector as Vector

data Answer = Success Object | Error Object
  deriving Show

data Request = Request
  { requestMessageID  :: Int
  , requestMethod     :: Text
  , requestParams     :: Vector Object }
  deriving Show

putRequest :: Request -> Put
putRequest (Request msgID method params) =
  MP.putArray id $ Vector.fromList $ [MP.putInt 0, MP.putInt msgID, MP.putStr method, MP.putArray put params]

data Message
  = Response
    { responseMessageID :: Int
    , responseAnswer    :: Answer }
  | Notification
    { notificationMethod :: Text
    , notificationParams :: Vector Object }
  deriving Show

getMessage :: Get Message
getMessage = do
  _ <- getWord8
  tpe <- MP.getInt
  case tpe of
    1 -> do
      id <- MP.getInt
      err <- get
      res <- get
      return $ Response id $ f err res where
        f ObjectNil res = Success res
        f err ObjectNil = Error err
    2 -> do
      method <- MP.getStr
      (ObjectArray params) <- get
      return $ Notification method params
    _ -> fail "unsupported message type"
