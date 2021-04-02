module Codec.Sarsi.SBT.Machine where

import Codec.Sarsi (Event(..), Message(..), Level(..))
import Codec.Sarsi.SBT (SBTEvent(..), eventParser, cleanEC, untilLineBreak, end)
import Data.Attoparsec.Text.Machine (processParser, streamParser)
import Data.Machine (ProcessT, (<~), asParts, auto, scan)
import Data.Text (Text)

import qualified Data.Text as Text

eventProcess :: Monad m => ProcessT m Text Event
eventProcess = asParts <~ auto unpack <~ (scan f (emptySession, Nothing)) <~ eventProcess'
  where
    f (session, _) event = runSession session event
    unpack (_, Just e) = [e]
    unpack _           = []

eventProcess' :: Monad m => ProcessT m Text SBTEvent
eventProcess' = asParts <~ auto unpackEvent <~ streamParser eventParser <~ preprocessing
  where
    preprocessing =
      asParts <~ auto unpackEC <~ processParser cleanEC <~ asParts <~ auto unpackLine <~ streamParser byLine
        where byLine = untilLineBreak <* end
    unpackEvent (Right e)  = [e]
    unpackEvent (Left _)  = []
    unpackEC (Left _)     = []
    unpackEC (Right (_, txt))  = [txt]
    unpackLine (Right txt)  = [txt `Text.snoc` '\n']
    unpackLine (Left _)     = []


data Session = Session { isBuilding :: Bool, counts :: (Int, Int) }
  deriving Show

emptySession :: Session
emptySession = Session False (0, 0)

runSession :: Session -> SBTEvent -> (Session, Maybe Event)
runSession (Session False f)     (CompileStart _)  = (Session True   f,            Just $ Start $ Text.pack "scala")
runSession (Session False f)     _                 = (Session False  f,            Nothing)
runSession (Session True f)      (CompileStart _)  = (Session True   f,            Nothing)
runSession (Session True (e,w))  (TaskFinish _ _)  = (Session False  (0, 0),       Just $ Finish e w)
runSession (Session True f)      (Throw msg)       = (Session True   (inc msg f),  Just $ Notify msg)
  where
    inc (Message _ Warning _) (e, w) = (e, w + 1)
    inc (Message _ Error _)   (e, w) = (e + 1, w)
