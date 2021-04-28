module Codec.Sarsi.SBT.Machine where

import Codec.Sarsi (Event (..), Level (..), Message (..), filePath)
import Codec.Sarsi.Curses (cleaningCurses)
import Codec.Sarsi.SBT (SBTEvent (..), cleaningCursesSBT, eventParser)
import Data.Attoparsec.Text.Machine (asLines, processParser, streamParser)
import Data.Machine (ProcessT, asParts, auto, filtered, scan, (<~))
import Data.Text (Text)
import qualified Data.Text as Text
import Sarsi (Topic (..))
import System.FilePath (makeRelative)

eventProcess :: Monad m => Topic -> ProcessT m Text Event
eventProcess t = asParts <~ auto unpack <~ (scan f (emptySession, Nothing)) <~ eventProcess'
  where
    f (session, _) event = runSession t session event
    unpack (_, Just e) = [e]
    unpack _ = []

eventProcess' :: Monad m => ProcessT m Text SBTEvent
eventProcess' = asParts <~ auto unpackEvent <~ streamParser eventParser <~ preprocessing
  where
    preprocessing = filtered (not . Text.null) <~ parsing cleaningCursesSBT <~ parsing cleaningCurses <~ input
      where
        input = auto (\txt -> Text.snoc txt '\n') <~ asLines
        parsing p = asParts <~ auto f <~ processParser p
          where
            f (Left _) = []
            f (Right (_, txt)) = [txt]
    unpackEvent (Right e) = [e]
    unpackEvent (Left _) = []

data Session = Session {isBuilding :: Bool, counts :: (Int, Int)}
  deriving (Show)

emptySession :: Session
emptySession = Session False (0, 0)

runSession :: Topic -> Session -> SBTEvent -> (Session, Maybe Event)
runSession _ (Session False f) (CompileStart _) = (Session True f, Just $ Start $ Text.pack "scala")
runSession _ (Session False f) _ = (Session False f, Nothing)
runSession _ (Session True f) (CompileStart _) = (Session True f, Nothing)
runSession _ (Session True (e, w)) (TaskFinish _ _) = (Session False (0, 0), Just $ Finish e w)
runSession (Topic _ _ root) (Session True f) (Throw (Message loc lvl txts)) =
  (Session True (inc lvl f), Just $ Notify msg)
  where
    inc Warning (e, w) = (e, w + 1)
    inc Error (e, w) = (e + 1, w)
    msg = (Message (loc {filePath = Text.pack $ makeRelative root (Text.unpack $ filePath loc)}) lvl txts)
