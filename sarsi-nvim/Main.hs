module Main where

import Codec.Sarsi (Event (..), Level (..), Location (..), Message (..))
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueue, readTBQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Monad (when)
import Data.Machine (ProcessT, asParts, auto, autoM, final, runT, runT_, scan, sinkPart_, (<~))
import Data.Machine.Fanout (fanout)
import Data.MessagePack (Object (..))
import qualified Data.MessagePack.RPC as RPC
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import NVIM.Client (CommandQueue, ask', mkConnection, send)
import NVIM.Command (Command (..))
import NVIM.QuickFix (toQuickFix)
import Sarsi (Topic (..), getBroker, getTopic, title)
import Sarsi.Consumer (consumeOrWait)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (Handle, IOMode (WriteMode))
import qualified System.IO as IO
import System.IO.Machine (sinkIO, sourceIO)

data BuildStatus = Starting | Building | Done
  deriving (Show, Eq)

data PluginAction = Focus | Next | Previous
  deriving (Bounded, Show, Enum, Eq, Ord, Read)

pluginActions :: [PluginAction]
pluginActions = [minBound ..]

data PluginState = PluginState
  { buildStatus :: BuildStatus,
    buildErrors :: Vector (Location, [Text]),
    buildWarnings :: Vector (Location, [Text]),
    focus :: Maybe (Level, Int),
    buffer :: Object,
    window :: Maybe Object
  }
  deriving (Show)

echo :: String -> Command
echo str = NvimCommand [ObjectStr . Text.pack $ concat ["echo \"", str, "\""]]

echom :: String -> Command
echom str = NvimCommand [ObjectStr . Text.pack $ concat ["echom \"", title, ": ", str, "\""]]

jumpTo :: Location -> [Command]
jumpTo loc =
  (\x -> NvimCommand [ObjectStr . Text.pack $ x])
    <$> [ concat ["drop +", show $ line loc, " ", Text.unpack $ filePath loc],
          concat ["call cursor(", show $ line loc, ", ", show $ column loc, ")"],
          "normal zz"
        ]

openLogFile :: Topic -> IO Handle
openLogFile (Topic _ fp) = IO.openFile (concat [fp, "-nvim.log"]) WriteMode

parseAction :: (Text, [Object]) -> PluginAction
parseAction (m, _) = read $ Text.unpack m

parseArgs :: [String] -> Either String Bool
parseArgs [] = Right False
parseArgs ["--log"] = Right True
parseArgs _ = Left "usage: [--log]"

pluginStateInit :: Object -> PluginState
pluginStateInit b = PluginState Done Vector.empty Vector.empty Nothing b Nothing

putLogLn :: Maybe Handle -> String -> IO ()
putLogLn Nothing _ = return ()
putLogLn (Just h) s = IO.hPutStrLn h s >> IO.hFlush h

registerActions :: [Command]
registerActions =
  (\x -> NvimCommand [ObjectStr . Text.pack $ concat ["command! Sarsi", x, " call rpcnotify(g:sarsi, '", x, "')"]])
    <$> show
    <$> pluginActions

update :: Monoid a => Maybe Handle -> CommandQueue -> TVar PluginState -> Event -> IO a
update h q s' e = do
  display e
  case e of
    (Start _) -> updateState (\s -> s {buildStatus = Starting})
    (Finish _ _) -> do
      emptyErrors <-
        atomically $
          stateTVar
            s'
            ( \s -> case buildStatus s of
                Building -> (Vector.null $ buildErrors s, s {buildStatus = Done})
                _ -> (True, s {buildStatus = Done, buildErrors = Vector.empty, buildWarnings = Vector.empty})
            )
      -- TODO Do auto-focus (only if users did not focus since Building)
      when emptyErrors $ windowClose q s'
    (Notify msg) ->
      updateState
        ( \s ->
            if buildStatus s /= Building
              then s {buildStatus = Building, focus = Nothing, buildErrors = Vector.empty, buildWarnings = Vector.empty}
              else s
        )
        >> updateMsg msg
  trace h
  return mempty
  where
    display (Start _) = nvim_ h q $ echom $ show e
    display (Finish _ _) = nvim_ h q $ echom $ show e
    display (Notify (Message loc lvl _)) = nvim_ h q $ echo $ concat [show loc, " ", show lvl]
    trace Nothing = return ()
    trace _ = do
      s <- readTVarIO s'
      putLogLn h $ show s
    updateMsg msg = atomically $ modifyTVar' s' (f msg)
      where
        f x s = g x
          where
            g (Message loc Error txts) = s {buildErrors = Vector.snoc es (loc, txts)}
            g (Message loc Warning txts) = s {buildWarnings = Vector.snoc ws (loc, txts)}
            (es, ws) = case buildStatus s of
              Starting -> (Vector.empty, Vector.empty)
              _ -> (buildErrors s, buildWarnings s)
    updateState f = atomically $ modifyTVar' s' f

-- TODO Wrap this in an appropriate transformer
nvim :: Maybe Handle -> CommandQueue -> Command -> IO (Maybe Object)
nvim hLog q cmd = do
  r <- ask' q cmd
  case r of
    RPC.Success a -> return $ Just a
    RPC.Error err -> do
      putLogLn hLog $ show err
      return Nothing

nvim_ :: Maybe Handle -> CommandQueue -> Command -> IO ()
nvim_ h q c = nvim h q c >> return ()

-- TODO NEW STUFF, move above
-- TODO Important: could they all be into STM? how to avoid unnecessary readTVarIO?
-- There must be a useful `Async + STM` atomic layer

bufferSetLines :: CommandQueue -> TVar PluginState -> [Text] -> IO ()
bufferSetLines q s' txts = do
  s <- readTVarIO s'
  let b = buffer s
  (RPC.Success _) <- ask' q $ NvimBufSetLines b 0 64 False txts
  return ()

windowClose :: CommandQueue -> TVar PluginState -> IO ()
windowClose q s' = do
  s <- readTVarIO s'
  case window s of
    Nothing -> return ()
    Just w -> do
      -- Tolerate failure if window was closed manually by user
      _ <- ask' q $ NvimWinClose w False
      atomically . modifyTVar' s' $ \x -> x {window = Nothing}
      return ()

bufferShow :: CommandQueue -> TVar PluginState -> Int -> IO ()
bufferShow q s' height = do
  windowClose q s'
  (RPC.Success (ObjectInt rows)) <- ask' q (NvimWinGetHeight $ ObjectInt 0)
  (RPC.Success (ObjectInt cols)) <- ask' q (NvimWinGetWidth $ ObjectInt 0)
  s <- readTVarIO s'
  (RPC.Success w) <- ask' q $ openWin (buffer s) rows cols
  atomically . modifyTVar' s' $ \x -> x {window = Just w}
  return ()
  where
    openWin b rows cols =
      NvimOpenWin
        b
        False
        ( ObjectMap $
            ( Vector.fromList
                [ (ObjectStr $ Text.pack "style", ObjectStr $ Text.pack "minimal"),
                  (ObjectStr $ Text.pack "relative", ObjectStr $ Text.pack "win"),
                  (ObjectStr $ Text.pack "row", ObjectInt $ rows - height),
                  (ObjectStr $ Text.pack "col", ObjectInt 0),
                  (ObjectStr $ Text.pack "width", ObjectInt cols),
                  (ObjectStr $ Text.pack "height", ObjectInt height)
                ]
            )
        )

actionFocus :: Maybe Handle -> CommandQueue -> TVar PluginState -> Level -> Int -> IO ()
actionFocus hLog q s' lvl rank = do
  s <- readTVarIO s'
  let (loc, txts) = focusContent lvl rank s
  _ <- traverse (nvim_ hLog q) $ jumpTo loc
  bufferSetLines q s' txts
  bufferShow q s' $ length txts
  return ()

actionMove :: Maybe Handle -> CommandQueue -> TVar PluginState -> (PluginState -> PluginState) -> IO ()
actionMove hLog q s' f = do
  fcs <-
    atomically $ do
      s <- readTVar s'
      let s'' = f s
      writeTVar s' s''
      return $ focus s''
  case fcs of
    Nothing -> return ()
    Just (lvl, rank) -> actionFocus hLog q s' lvl rank

focusContent :: Level -> Int -> PluginState -> (Location, [Text])
focusContent lvl rank s = Vector.unsafeIndex xs rank
  where
    xs = case lvl of
      Warning -> buildWarnings s
      Error -> buildErrors s

focusDefault :: PluginState -> Maybe (Level, Int)
focusDefault s = select (Vector.null $ buildErrors s) (Vector.null $ buildWarnings s)
  where
    select False _ = Just (Error, 0)
    select True False = Just (Warning, 0)
    select True True = Nothing

focusMove :: Int -> PluginState -> PluginState
focusMove i s =
  case focus s of
    Nothing -> s {focus = focusDefault s}
    Just (lvl, rank) -> s {focus = Just $ f lvl (rank + i)}
  where
    f lvl rank | rank < 0 = f (toggle lvl) ((Vector.length $ select lvl) + rank)
    f lvl rank | rank >= (Vector.length $ select lvl) = f (toggle lvl) (rank - (Vector.length $ select lvl))
    f lvl rank = (lvl, rank)
    toggle lvl | Vector.null $ select (toggle' lvl) = lvl
    toggle lvl = toggle' lvl
    toggle' Warning = Error
    toggle' Error = Warning
    select Warning = buildWarnings s
    select Error = buildErrors s

-- TODO How to make it shudown gracefully? currently it's probably killed by nvim while blocking in `consumerOrWait`
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      putStrLn err
      exitWith $ ExitFailure 1
    Right logging -> do
      IO.hSetBuffering IO.stdin IO.NoBuffering
      IO.hSetBuffering IO.stdout IO.NoBuffering
      b <- getBroker
      t <- getTopic b "."
      hLog <- if logging then Just <$> (openLogFile t) else return Nothing
      qCmds <- atomically $ newTBQueue 8
      qNotifs <- atomically $ newTBQueue 8
      connClose <- mkConnection IO.stdin IO.stdout qCmds qNotifs (errHandler hLog)
      (Just buf) <- nvim hLog qCmds $ NvimCreateBuf False True
      state <- atomically $ newTVar $ pluginStateInit buf
      notifier <-
        async . runT_ $
          autoM (notify hLog qCmds state) <~ (auto parseAction) <~ (sourceIO . atomically $ readTBQueue qNotifs)
      _ <- traverse (nvim_ hLog qCmds) registerActions
      putLogLn hLog "ready"
      _ <- consumeOrWait t (consumer hLog state qCmds)
      cancel notifier
      connClose
      _ <- traverse IO.hClose hLog
      return ()
  where
    errHandler hLog err = do
      putLogLn hLog $ show err
    notify hLog q s' Focus = do
      s <- readTVarIO s'
      case focus s of
        Nothing ->
          case focusDefault s of
            Nothing -> nvim_ hLog q $ echom "nothing to fix"
            Just (lvl, rank) -> do
              atomically . modifyTVar' s' $ \x -> x {focus = Just (lvl, rank)}
              actionFocus hLog q s' lvl rank
        Just (lvl, rank) -> actionFocus hLog q s' lvl rank
    notify hLog q s' Next = actionMove hLog q s' (focusMove 1)
    notify hLog q s' Previous = actionMove hLog q s' (focusMove (-1))
    consumer h s q Nothing src = consumer h s q (Just 0) src
    consumer h s q (Just i) src = do
      i' <- runT $ final <~ asParts <~ fanout [quickFixes, pluginUpdate] <~ src
      return (Left $ head i')
      where
        quickFixes = auto (\x -> [x]) <~ sinkPart_ id (sinkIO (send q) <~ asParts) <~ toQuickFixes i
        pluginUpdate = autoM (update h q s)

    toQuickFixes :: Int -> ProcessT IO Event (Int, [Command])
    toQuickFixes acc = scan f (acc, [])
      where
        f (i, _) event = toQuickFix i event
