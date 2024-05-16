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
    HCEnv (..),
    withConfig,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader.Class (MonadReader (..), asks)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Kind
import HConf.Config (Config, getPackages, getVersion)
import HConf.Env (Env)
import HConf.Utils (Name)
import HConf.Version (Version)

data HCEnv = HCEnv
  { config :: Config,
    env :: Env
  }

newtype ConfigT (a :: Type)
  = ConfigT {_runConfigT :: ReaderT HCEnv IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader HCEnv,
      MonadIO,
      MonadUnliftIO,
      MonadFail
    )

runConfigT :: ConfigT a -> Env -> Config -> IO a
runConfigT (ConfigT (ReaderT f)) env config = f HCEnv {..}

packages :: ConfigT [Name]
packages = getPackages <$> asks config

version :: ConfigT Version
version = getVersion <$> asks config

withConfig :: (Config -> t -> t) -> t -> ConfigT t
withConfig f t = flip f t <$> asks config
