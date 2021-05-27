module Sarsi.Processor where

import qualified Codec.GHC.Log as GHC
import Codec.Sarsi (Message)
import qualified Codec.Sarsi.Dotnet as Dotnet
import Codec.Sarsi.GHC (fromGHCLog)
import qualified Codec.Sarsi.Nix as Nix
import qualified Codec.Sarsi.Rust as Rust
import qualified Codec.Sarsi.SBT.Machine as SBT
import qualified Codec.Sarsi.Scala as Scala
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine (ProcessT, asParts, auto, flattened, (<~))
import Data.Machine.Fanout (fanout)
import Data.Text (Text)
import Rosetta (LanguageTag (..), ProjectTag (..), languageTags, projectLanguages, projectTags)
import Sarsi (Topic (..))

projectProcesses :: ProjectTag -> [Topic -> ProcessT IO Text Message]
projectProcesses DOTNET = [(const $ processMessage Dotnet.messageParser)]
projectProcesses project = g =<< (f <$> projectLanguages project)
  where
    f l = unpack <$> languageProcess l
      where
        unpack (Right p) = [(l, p)]
        unpack (Left _) = []
    g (Just [(_, p)]) = [p]
    g _ = []

languageProcess :: LanguageTag -> Maybe (Either ProjectTag (Topic -> ProcessT IO Text Message))
languageProcess CS = Just . Left $ DOTNET
languageProcess FS = Just . Left $ DOTNET
languageProcess HS = Just . Right $ const processHaskell
languageProcess NX = Just . Right . const $ processMessage Nix.messageParser
languageProcess RS = Just . Right . const $ processMessage Rust.messageParser
languageProcess SC = Just . Right $ \(Topic _ _ root) -> (processMessage $ Scala.messageParser root) <~ SBT.cleaningCursesSBT
languageProcess _ = Nothing

processAll :: [Topic -> ProcessT IO Text Message] -> Topic -> ProcessT IO Text Message
processAll xs t = flattened <~ (fanout $ (\p -> (auto (\x -> [x])) <~ p t) <$> xs)

processAny :: Topic -> ProcessT IO Text Message
processAny = processAll $ concat [ls, ps]
  where
    ls = languageTags >>= (unpack . languageProcess)
      where
        unpack (Just (Right f)) = [f]
        unpack _ = []
    ps = projectTags >>= projectProcesses

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
