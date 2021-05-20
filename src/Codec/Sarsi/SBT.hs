{-# LANGUAGE OverloadedStrings #-}

module Codec.Sarsi.SBT where

import Codec.Sarsi (Level (..), Location (..), Message (..))
import Codec.Sarsi.Scala
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AttoText
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (makeRelative)

data SBTEvent = CompileStart Text | TaskFinish Bool Text | Throw Message
  deriving (Show)

cleaningCursesSBT :: Parser Text
cleaningCursesSBT = choice [silent, empty, keep]
  where
    silent = "  | =>" >> untilLineBreak >> "\n" >> return Text.empty
    empty = do
      _ <- AttoText.takeWhile1 $ \w -> w == '\n'
      return Text.empty
    keep = do
      content <- (AttoText.takeWhile1 $ \w -> w /= '\n') <* end
      return $ content `Text.snoc` '\n'

eventParser :: FilePath -> Parser SBTEvent
eventParser root = choice [compile, finish, Throw <$> messageParser root]
  where
    compile = do
      txt <- string "[info] " *> choice ["Build triggered", "Compiling", "compiling"] *> untilLineBreak <* end
      return $ CompileStart txt
    finish = do
      res <- status <* space
      txt <- string "Total time: " *> untilLineBreak <* end
      _ <- end
      return $ TaskFinish res txt
      where
        status = choice [string "[success]" *> return True, string "[error]" *> return False]
