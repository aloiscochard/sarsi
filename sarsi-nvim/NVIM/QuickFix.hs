module NVIM.QuickFix where

import Codec.Sarsi (Event (..), Level (..), Location (..), Message (..))
import Data.MessagePack (Object (..))
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import NVIM.Command (Command (NvimCallFunction))

toQuickFix :: Int -> Event -> (Int, [Command])
toQuickFix _ (Start _) = (0, [])
toQuickFix i (Finish _ _) = (0, (if i == 0 then [setqflistEmpty] else []))
toQuickFix i (Notify msg@(Message _ _ _)) = (i + 1, [setqflist (if i == 0 then "r" else "a") [mkQuickFix msg]])

-- TODO Sanitize text description by escaping special characters
mkQuickFix :: Message -> Object
mkQuickFix (Message (Location fp col ln) lvl txts) =
  ObjectMap $
    ( Vector.fromList
        [ (ObjectStr $ Text.pack "filename", ObjectStr fp),
          (ObjectStr $ Text.pack "lnum", ObjectInt ln),
          (ObjectStr $ Text.pack "col", ObjectInt col),
          (ObjectStr $ Text.pack "type", ObjectStr . Text.pack $ tpe lvl),
          (ObjectStr $ Text.pack "text", ObjectStr $ head txts)
        ]
    )
  where
    tpe Error = "E"
    tpe Warning = "W"

setqflist :: String -> [Object] -> Command
setqflist action items =
  NvimCallFunction
    (Text.pack "setqflist")
    [ ObjectArray (Vector.fromList items),
      ObjectStr $ Text.pack action
    ]

setqflistEmpty :: Command
setqflistEmpty = setqflist "r" []
