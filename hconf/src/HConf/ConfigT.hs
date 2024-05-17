{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.ConfigT
  ( ConfigT (..),
    packages,
    version,
    runConfigT,
    HCEnv (..),
    withConfig,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Kind
import HConf.Config (Config, getPackages, getVersion)
import HConf.Env (Env)
import HConf.Log (Log (..))
import HConf.Utils (Name)
import HConf.Version (Version)
import Relude

data HCEnv = HCEnv
  { config :: Config,
    env :: Env,
    indention :: Int
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
runConfigT (ConfigT (ReaderT f)) env config = f HCEnv {indention = 0, ..}

packages :: ConfigT [Name]
packages = getPackages <$> asks config

version :: ConfigT Version
version = getVersion <$> asks config

withConfig :: (Config -> t -> t) -> t -> ConfigT t
withConfig f t = flip f t <$> asks config

indent :: Int -> String
indent i = replicate (i * 2) ' '

instance Log ConfigT where
  log txt = do
    i <- asks indention
    liftIO $ putStrLn $ indent i <> txt
  inside = local (\c -> c {indention = indention c + 1})