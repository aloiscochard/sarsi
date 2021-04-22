module NVIM.Command where

import Data.MessagePack (Object (..))
import Data.MessagePack.RPC (Request (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- TODO Support special types (with newtype over Object) for Buffer, Window, Tabpage (EXT 0, 1, 2)
-- see https://neovim.io/doc/user/api.html
data Command
  = NvimBufSetLines Object Int Int Bool [Text]
  | NvimCommand [Object]
  | NvimCallFunction Text [Object]
  | NvimCreateBuf Bool Bool -- {listed} {scratch}
  | NvimOpenWin Object Bool Object
  | NvimWinClose Object Bool
  | NvimWinGetBuf Object
  | NvimWinGetHeight Object
  | NvimWinGetWidth Object

mkRequest :: Int -> Command -> Request
mkRequest msgId (NvimBufSetLines b s e strict xs) =
  Request msgId (Text.pack "nvim_buf_set_lines") [b, ObjectInt s, ObjectInt e, ObjectBool strict, ObjectArray (Vector.fromList (ObjectStr <$> xs))]
mkRequest msgId (NvimCommand xs) =
  Request msgId (Text.pack "nvim_command") xs
mkRequest msgId (NvimCallFunction m xs) =
  Request msgId (Text.pack "nvim_call_function") [ObjectStr m, ObjectArray (Vector.fromList xs)]
mkRequest msgId (NvimCreateBuf l s) =
  Request msgId (Text.pack "nvim_create_buf") [ObjectBool l, ObjectBool s]
mkRequest msgId (NvimOpenWin b e cfg) =
  Request msgId (Text.pack "nvim_open_win") [b, ObjectBool e, cfg]
mkRequest msgId (NvimWinClose w force) =
  Request msgId (Text.pack "nvim_win_close") [w, ObjectBool force]
mkRequest msgId (NvimWinGetBuf w) =
  Request msgId (Text.pack "nvim_win_get_buf") [w]
mkRequest msgId (NvimWinGetHeight w) =
  Request msgId (Text.pack "nvim_win_get_height") [w]
mkRequest msgId (NvimWinGetWidth w) =
  Request msgId (Text.pack "nvim_win_get_width") [w]
