{-# LANGUAGE OverloadedStrings #-}

module Codec.ELM.Log where

import Data.Aeson
import Data.Text.Encoding (encodeUtf8)
import System.IO (Handle)
import Text.PrettyPrint.ANSI.Leijen
    ( Doc, (<>), displayS, displayIO, dullcyan, fillSep
    , hardline, plain, renderPretty, text, list
    )

import qualified Data.List as List
import qualified Data.Text as Text

data Position =
  Position
    { line :: Int
    , column :: Int
    } deriving (Show)

instance FromJSON Position where
  parseJSON (Object v) =
    Position <$> v .: "line"
             <*> v .: "column"

data Region =
  Region
    { start :: Position
    , end :: Position
    } deriving (Show)

instance FromJSON Region where
  parseJSON (Object v) =
    Region <$> v .: "start"
           <*> v .: "end"

data Message=
  Message
    { tag  :: String
    , overview   :: String
    , details   :: String
    , region  :: Region
    , subRegion  :: Maybe Region
    , file   :: String
    --TODO Use an algebraic data type for tpe
    , tpe   :: String
    } deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "tag"
            <*> v .: "overview"
            <*> v .: "details"
            <*> v .: "region"
            <*> v .:? "subregion"
            <*> v .: "file"
            <*> v .: "type"

decodeMessage :: Text.Text -> Either String [Message]
decodeMessage text = eitherDecodeStrict $ encodeUtf8 text

toDoc :: Message -> Doc
toDoc message  =
    messageBar (tag message) (file message)
    <> hardline <> hardline <>
     text (overview message)
    <> hardline <> hardline <>
    text (details message)
    <> hardline

toHandle :: Handle -> Message -> IO ()
toHandle handle message =
  displayIO
    handle
    (renderPretty 1 80  (toDoc  message))

messageBar :: String -> String -> Doc
messageBar tag file =
  let
    usedSpace =
      4 + length tag + 1 + length file
  in
    dullcyan $ text $
      "-- " ++ tag ++ " " ++ replicate (max 1 (80 - usedSpace)) '-' ++ " " ++ file
