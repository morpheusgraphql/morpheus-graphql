{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module HConf.ConfigT (ConfigT (..)) where

import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Kind
import HConf.Config (Config)

newtype
  ConfigT
    (m :: Type -> Type)
    (a :: Type)
  = ConfigT {runConfigT :: ReaderT Config m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config
    )
