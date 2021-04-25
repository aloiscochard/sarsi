module Sarsi.Processor where

import qualified Codec.GHC.Log as GHC
import Codec.Sarsi (Message)
import Codec.Sarsi.GHC (fromGHCLog)
import qualified Codec.Sarsi.Rust as Rust
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine (ProcessT, asParts, auto, filtered, flattened, (<~))
import Data.Machine.Fanout (fanout)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Rosetta (LanguageTag (..), ProjectTag, projectLanguages)

data Processor = Processor {language :: LanguageTag, process :: ProcessT IO Text Message}

instance Eq Processor where
  a == b = (language a) == (language b)

instance Ord Processor where
  compare a b = compare (language a) (language b)

projectProcessors :: ProjectTag -> Set Processor
-- projectProcessors DOTNET = processDotnet
projectProcessors project = Set.fromList $ g =<< f <$> projectLanguages project
  where
    f l = (\p -> (l, p)) <$> languageProcess l
    g (Just (l, p)) = [Processor {language = l, process = p}]
    g Nothing = []

languageProcess :: LanguageTag -> Maybe (ProcessT IO Text Message)
languageProcess HS = Just processHaskell
languageProcess RS = Just processRust
languageProcess _ = Nothing

processAll :: [ProcessT IO Text Message] -> ProcessT IO Text Message
processAll xs = flattened <~ (fanout $ (\p -> (auto (\x -> [x])) <~ p) <$> xs)

processAny :: ProcessT IO Text Message
processAny = processAll [processHaskell, processRust]

processHaskell :: ProcessT IO Text Message
processHaskell = asParts <~ auto unpack <~ streamParser GHC.messageParser
  where
    unpack (Right msg) = [fromGHCLog msg]
    unpack (Left _) = []

processRust :: ProcessT IO Text Message
processRust = asParts <~ auto unpack <~ streamParser Rust.messageParser <~ filtered (not . Text.null)
  where
    unpack (Right msg) = [msg]
    unpack (Left _) = []
