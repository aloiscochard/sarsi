{-# LANGUAGE Rank2Types #-}

module Data.Attoparsec.Machine where

import Data.Attoparsec.Internal.Types (IResult (..))
import Data.Machine (Is (Refl), MachineT (..), ProcessT, Step (Await, Yield), stopped)

streamParserWith :: (Monoid i, Monad m) => (i -> IResult i a) -> ProcessT m i (Either String a)
streamParserWith runParser = start
  where
    start = MachineT . return $ Await parse Refl stopped
    parse i = MachineT . return . f $ runParser i
    f (Fail _ _ e) = Yield (Left e) start
    f (Partial c) = Await (MachineT . return . f . c) Refl $ (MachineT . return . f $ c mempty)
    f (Done i r) = Yield (Right r) (parse i)

processParserWith :: (Monoid i, Monad m) => (i -> IResult i a) -> ProcessT m i (Either String (i, a))
processParserWith runParser = MachineT . return $ Await parse Refl stopped
  where
    parse i = MachineT . return . f $ runParser i
    f (Fail _ _ e) = Yield (Left e) (processParserWith runParser)
    f (Partial c) = f $ c mempty
    f (Done i a) = Yield (Right (i, a)) (processParserWith runParser)
