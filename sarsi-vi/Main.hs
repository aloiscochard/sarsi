module Main where

import Codec.Sarsi (Event (..), Level (..), Location (..), Message (..))
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import qualified Data.List as List
import Data.Machine (ProcessT, asParts, final, runT, scan, sinkPart_, (<~))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.IO (hPutStrLn)
import Sarsi (Topic (..), getBroker, getTopic)
import qualified Sarsi as Sarsi
import Sarsi.Consumer (consumeOrWait)
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, openFile)
import System.IO.Machine (sinkIO)

-- TODO Remove the MVar by impl. scanM ((a -> b -> m a) -> a -> ProcessT m (k b) a) in machines

title :: String
title = concat [Sarsi.title, "-vi"]

-- :set efm=%f:%l:%c:%t\ %m
toVi :: Message -> Text
toVi (Message (Location fp col ln) lvl txts) = Text.concat [header, pack " ", body]
  where
    header = Text.concat $ List.intersperse (pack ":") [fp, pack $ show ln, pack $ show col, tpe lvl]
    body = Text.concat $ List.intersperse (pack " ") txts
    tpe Error = pack "e"
    tpe Warning = pack "w"

data Action = Append | Replace deriving (Show)

data LogEvent = LogEvent Action Text deriving (Show)

data Command = Throw LogEvent | Echo String deriving (Show)

convert :: Bool -> Event -> (Bool, [Command])
convert first (Notify msg@(Message loc lvl _)) = (False, [Throw e, Echo $ concat [show loc, " ", show lvl]])
  where
    e = LogEvent mode $ toVi msg
    mode = if first then Replace else Append
convert _ e = (True, [Echo $show e])

converter :: Bool -> ProcessT IO Event (Bool, [Command])
converter first = scan f (first, []) where f (first', _) event = convert first' event

dump :: FilePath -> Maybe Handle -> Command -> IO (Maybe Handle)
dump _ h (Echo txt) = (putStrLn $ concat [title, ": ", txt]) >> return h
dump fp Nothing e = openFile fp WriteMode >>= \h -> dump fp (Just h) e
dump _ (Just h) (Throw (LogEvent Append txt)) = fmap (const $ Just h) $ (hPutStrLn h txt >> hFlush h)
dump fp (Just h) (Throw (LogEvent Replace txt)) = hClose h >> dump fp Nothing (Throw $ LogEvent Append txt)

writer :: FilePath -> MVar (Maybe Handle) -> Command -> IO ()
writer tp var c = do
  h <- readMVar var
  h' <- dump fp h c
  _ <- swapMVar var h'
  return ()
  where
    fp = tp ++ ".vi"

main :: IO ()
main = do
  b <- getBroker
  t <- getTopic b "."
  var <- newMVar Nothing
  consumeOrWait t $ consumer t var
  where
    consumer (Topic _ tp) var first src = do
      res <- runT $ final <~ sinkPart_ id ((sinkIO $ writer tp var) <~ asParts) <~ converter (maybe True id first) <~ src
      return $ Left $ head res
