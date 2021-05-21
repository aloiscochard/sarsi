module Codec.Sarsi.SBT.Machine where

import Codec.Sarsi (Event (..), Level (..), Message (..))
import Codec.Sarsi.Curses (cleaningCurses)
import Codec.Sarsi.SBT (SBTEvent (..), cleaningCursesSBT, eventParser)
import Data.Attoparsec.Text.Machine (asLines, processParser, streamParser)
import Data.Machine (ProcessT, asParts, auto, filtered, scan, (<~))
import Data.Text (Text)
import qualified Data.Text as Text
import Sarsi (Topic (..))

eventProcess :: Monad m => Topic -> ProcessT m Text Event
eventProcess t = asParts <~ auto unpack <~ (scan f (emptySession, Nothing)) <~ eventProcess' t
  where
    f (session, _) event = runSession session event
    unpack (_, Just e) = [e]
    unpack _ = []

eventProcess' :: Monad m => Topic -> ProcessT m Text SBTEvent
eventProcess' (Topic _ _ root) = asParts <~ auto unpackEvent <~ streamParser (eventParser root) <~ preprocessing
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

runSession :: Session -> SBTEvent -> (Session, Maybe Event)
runSession (Session False f) (CompileStart _) = (Session True f, Just $ Start $ Text.pack "scala")
runSession (Session False f) _ = (Session False f, Nothing)
runSession (Session True f) (CompileStart _) = (Session True f, Nothing)
runSession (Session True (e, w)) (TaskFinish _ _) = (Session False (0, 0), Just $ Finish e w)
runSession (Session True f) (Throw msg@(Message _ lvl _)) = (Session True (inc lvl f), Just $ Notify msg)
  where
    inc Warning (e, w) = (e, w + 1)
    inc Error (e, w) = (e + 1, w)
