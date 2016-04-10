module Data.MessagePack.ToJSON where

import Data.Aeson (Value(..))
import Data.Foldable
import Data.MessagePack (Object(..))

import qualified Data.HashMap.Strict as HM

toJSON :: Object -> Value
toJSON (ObjectBin _) = error "cannot convert binary field to JSON."
toJSON (ObjectExt _ _) = error "cannot convert extension field to JSON."
toJSON (ObjectNil) = Null
toJSON (ObjectBool b) = Bool b
toJSON (ObjectInt i) = Number $ fromIntegral i
toJSON (ObjectFloat f) = Number . fromRational $ toRational f
toJSON (ObjectDouble d) = Number . fromRational $ toRational d
toJSON (ObjectStr txt) = String txt
toJSON (ObjectArray obs) = Array $ fmap toJSON obs
toJSON (ObjectMap xs) = Object $ foldl' f HM.empty xs where
  f hm (k, v) = HM.insert (key k) (toJSON v) hm
  key (ObjectStr txt) = txt
  key _               = error "cannot convert non text key to JSON."
