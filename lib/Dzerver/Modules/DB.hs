{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Dzerver.Modules.DB where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.TH
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Map (Map, insertWith, lookup)
import Data.Maybe
import System.IO
import Prelude hiding (lookup)

data DB i v r where
  WriteDB :: i -> v -> DB i v ()
  ReadDB :: i -> DB i v [v]

makeEffect ''DB

class Indexable i where
  toIndex :: i -> ByteString

class Storable v where
  toStore :: v -> ByteString
  fromStore :: ByteString -> v

runFileDB :: (Indexable i, Storable v, LastMember IO effs) => FilePath -> Eff (DB i v : effs) ~> Eff effs
runFileDB fp = interpret $ \case
  WriteDB i v -> liftIO $ withFile fp WriteMode $ \h -> return ()
  ReadDB i -> liftIO $ withFile fp ReadMode $ \h -> return []

runPureDB :: (Indexable i, Storable v, Member (State (Map ByteString [ByteString])) effs) => Eff (DB i v : effs) ~> Eff effs
runPureDB = interpret $ \case
  WriteDB i v -> modify (insertWith (++) (toIndex i) [toStore v])
  ReadDB i -> gets (maybe [] (map fromStore) . lookup (toIndex i))
