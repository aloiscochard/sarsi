module Codec.Sarsi where

import Data.Text (Text, unpack)
import Data.Binary (Get, Put)
import Data.Vector (Vector)

import qualified Data.MessagePack.Get as Get
import qualified Data.MessagePack.Put as Put
import qualified Data.Text as Text

data Event
  = Start { label :: Text }
  | Finish { errors :: Int, warnings :: Int }
  | Notify { message :: Message }

instance Show Event where
  show (Start label) = concat ["starting ", unpack label, " build"]
  show (Finish 0 0) = "build success"
  show (Finish w 0) = concat ["build success with ", show w, " warning(s)"]
  show (Finish 0 e) = concat ["build fail with ", show e, " error(s)"]
  show (Finish w e) = concat ["build fail with ", show e, " error(s) and ", show w, " warning(s)"]
  show (Notify msg) = concat ["message=", show msg]

getEvent :: Get Event
getEvent = do
  tpe <- Get.getInt
  case tpe of
    0 -> Start <$> Get.getStr
    1 -> Finish <$> Get.getInt <*> Get.getInt
    2 -> Notify <$> getMessage
    _ -> fail "unsupported event type"

putEvent :: Event -> Put
putEvent (Start t) = Put.putInt 0 *> Put.putStr t
putEvent (Finish es ws) = Put.putInt 1 *> Put.putInt es *> Put.putInt ws
putEvent (Notify m) = Put.putInt 2 *> putMessage m

data Message = Message Location Level (Vector Text)

instance Show Message where
  show (Message loc lvl texts) = concat [show loc, " ", show lvl]

getMessage :: Get Message
getMessage = Message <$> getLocation <*> getLevel <*> Get.getArray Get.getStr

putMessage :: Message -> Put
putMessage (Message loc lvl txt) = putLocation loc *> putLevel lvl *> Put.putArray Put.putStr txt

data Location = Location { filePath :: Text, column :: Int, line :: Int }

instance Show Location where
  show (Location fp c l) = concat [Text.unpack fp, "@", show l, ":", show c]

getLocation :: Get Location
getLocation = Location <$> Get.getStr <*> Get.getInt <*> Get.getInt

putLocation :: Location -> Put
putLocation (Location fp c l) = Put.putStr fp *> Put.putInt c *> Put.putInt l

data Level = Warning | Error
  deriving (Enum, Show)

getLevel :: Get Level
getLevel = fmap toEnum Get.getInt

putLevel :: Level -> Put
putLevel = Put.putInt . fromEnum

