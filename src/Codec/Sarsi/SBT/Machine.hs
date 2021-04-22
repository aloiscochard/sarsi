module Codec.Sarsi.SBT.Machine where

import Codec.Sarsi (Event (..), Level (..), Message (..))
import Codec.Sarsi.Curses (cleaningCurses)
import Codec.Sarsi.SBT (SBTEvent (..), cleaningCursesSBT, eventParser)
import Data.Attoparsec.Text.Machine (asLines, processParser, streamParser)
import Data.Machine (ProcessT, asParts, auto, filtered, scan, (<~))
import Data.Text (Text)
import qualified Data.Text as Text

eventProcess :: Monad m => ProcessT m Text Event
eventProcess = asParts <~ auto unpack <~ (scan f (emptySession, Nothing)) <~ eventProcess'
  where
    f (session, _) event = runSession session event
    unpack (_, Just e) = [e]
    unpack _ = []

eventProcess' :: Monad m => ProcessT m Text SBTEvent
eventProcess' = asParts <~ auto unpackEvent <~ streamParser eventParser <~ preprocessing
  where
    preprocessing =
      filtered (not . Text.null)
        <~ asParts
        <~ auto unpackEC
        <~ processParser cleaningCursesSBT
        <~ asParts
        <~ auto unpackEC
        <~ processParser cleaningCurses
        <~ auto unlined
        <~ asLines
    unlined txt = Text.snoc txt '\n'
    unpackEvent (Right e) = [e]
    unpackEvent (Left _) = []
    unpackEC (Left _) = []
    unpackEC (Right (_, txt)) = [txt]

data Session = Session {isBuilding :: Bool, counts :: (Int, Int)}
  deriving (Show)

emptySession :: Session
emptySession = Session False (0, 0)

runSession :: Session -> SBTEvent -> (Session, Maybe Event)
runSession (Session False f) (CompileStart _) = (Session True f, Just $ Start $ Text.pack "scala")
runSession (Session False f) _ = (Session False f, Nothing)
runSession (Session True f) (CompileStart _) = (Session True f, Nothing)
runSession (Session True (e, w)) (TaskFinish _ _) = (Session False (0, 0), Just $ Finish e w)
runSession (Session True f) (Throw msg) = (Session True (inc msg f), Just $ Notify msg)
  where
    inc (Message _ Warning _) (e, w) = (e, w + 1)
    inc (Message _ Error _) (e, w) = (e + 1, w)
