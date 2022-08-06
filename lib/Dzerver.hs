{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Dzerver where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Lens

data DzerverConfig = DzerverConfig
  { _logLevel :: String,
    _port :: Int
  }
  deriving (Show, Eq)

makeLenses ''DzerverConfig

data DzerverState = DzerverState
  { _someThing :: Bool,
    _someThingOther :: Bool
  }
  deriving (Show, Eq)

makeLenses ''DzerverState

type DzerverEffects = [Reader DzerverConfig, State DzerverState]

newtype Dzerver a = Dzerver {runDzerver :: Eff DzerverEffects a}
