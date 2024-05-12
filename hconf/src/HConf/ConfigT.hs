{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module HConf.ConfigT
  ( ConfigT (..),
    packages,
    version,
    runConfigT,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader.Class (MonadReader (..), asks)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Kind
import HConf.Config (Config, getPackages, getVersion)
import HConf.Utils (Name)
import HConf.Version (Version)

newtype ConfigT (a :: Type)
  = ConfigT {_runConfigT :: ReaderT Config IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadIO,
      MonadUnliftIO,
      MonadFail
    )

runConfigT :: ConfigT a -> Config -> IO a
runConfigT (ConfigT (ReaderT f)) config = f config

packages :: ConfigT [Name]
packages = getPackages <$> asks id

version :: ConfigT Version
version = getVersion <$> asks id