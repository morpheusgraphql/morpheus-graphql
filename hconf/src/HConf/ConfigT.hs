{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
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

import Control.Exception (tryJust)
import Data.Kind
import HConf.Config.Config (Config, getPackages, getVersion)
import HConf.Env (Env)
import HConf.Utils (Name)
import HConf.Utils.Log (Log (..))
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
      MonadFail
    )

printException :: SomeException -> String
printException = show

runConfigT :: ConfigT a -> Env -> Config -> IO (Either String a)
runConfigT (ConfigT (ReaderT f)) env config = tryJust (Just . printException) (f HCEnv {indention = 0, ..})

packages :: ConfigT [Name]
packages = getPackages <$> asks config

version :: ConfigT Version
version = getVersion <$> asks config

withConfig :: (Config -> t -> ConfigT t') -> t -> ConfigT t'
withConfig f t = asks config >>= flip f t

indent :: Int -> String
indent i = replicate (i * 2) ' '

instance Log ConfigT where
  log txt = do
    i <- asks indention
    liftIO $ putStrLn $ indent i <> txt
  inside = local (\c -> c {indention = indention c + 1})