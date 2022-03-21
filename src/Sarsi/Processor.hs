module Sarsi.Processor where

import qualified Codec.GHC.Log as GHC
import Codec.Sarsi (Message)
import qualified Codec.Sarsi.Curses as Curses
import qualified Codec.Sarsi.GCC as GCC
import Codec.Sarsi.GHC (fromGHCLog)
import qualified Codec.Sarsi.Nix as Nix
import qualified Codec.Sarsi.Rust as Rust
import qualified Codec.Sarsi.SBT.Machine as SBT
import qualified Codec.Sarsi.Scala as Scala
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine (ProcessT, asParts, auto, flattened, (<~))
import Data.Machine.Fanout (fanout)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Rosetta (LanguageTag (..), ProjectTag, languageTags, projectLanguages)
import Sarsi (Topic (..))

data Processor = Processor {language :: LanguageTag, process :: Topic -> ProcessT IO Text Message}

instance Eq Processor where
  a == b = (language a) == (language b)

instance Ord Processor where
  compare a b = compare (language a) (language b)

projectProcessors :: ProjectTag -> Set Processor
projectProcessors project = Set.fromList $ g =<< f <$> projectLanguages project
  where
    f l = (\p -> (l, p)) <$> languageProcess l
    g (Just (l, p)) = [Processor {language = l, process = p}]
    g Nothing = []

languageProcess :: LanguageTag -> Maybe (Topic -> ProcessT IO Text Message)
languageProcess CC = Just . const $ processMessage $ GCC.messageParser
languageProcess HS = Just $ const processHaskell
languageProcess NX = Just . const $ processMessage Nix.messageParser
languageProcess RS = Just . const $ processMessage Rust.messageParser
languageProcess SC = Just $ \(Topic _ _ root) -> (processMessage $ Scala.messageParser root) <~ SBT.cleaningCursesSBT
languageProcess _ = Nothing

processAll :: [Topic -> ProcessT IO Text Message] -> Topic -> ProcessT IO Text Message
processAll xs t = flattened <~ (fanout $ (\p -> (auto (\x -> [x])) <~ p t) <$> xs)

processAny :: Topic -> ProcessT IO Text Message
processAny = processAll $ languageTags >>= (maybeToList . languageProcess)

processHaskell :: ProcessT IO Text Message
processHaskell = asParts <~ auto unpack <~ streamParser GHC.messageParser
  where
    unpack (Right msg) = [fromGHCLog msg]
    unpack (Left _) = []

processMessage :: Parser Message -> ProcessT IO Text Message
processMessage parser = asParts <~ auto unpack <~ streamParser parser
  where
    unpack (Right msg) = [msg]
    unpack (Left _) = []
