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
    info,
    warn,
    alert,
    infoList,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Kind
import HConf.Config (Config, getPackages, getVersion)
import HConf.Env (Env)
import HConf.Utils (Name)
import HConf.Version (Version)
import Relude

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

errorColor :: String
errorColor = "\x1b[31m"

successColor :: String
successColor = "\x1b[32m"

warningColor :: String
warningColor = "\x1b[33m"

noneColor :: String
noneColor =
  "\x1b[0m"

infoList :: (ToString a) => String -> [a] -> ConfigT ()
infoList label list = info (intercalate "\n -" (label : map toString list))

info :: String -> ConfigT ()
info = liftIO . putStrLn . withColor successColor

warn :: String -> ConfigT ()
warn = liftIO . putStrLn . withColor warningColor

alert :: String -> ConfigT ()
alert = liftIO . putStrLn . withColor errorColor

withColor :: String -> String -> String
withColor color x = color <> x <> noneColor

runConfigT :: ConfigT a -> Env -> Config -> IO a
runConfigT (ConfigT (ReaderT f)) env config = f HCEnv {..}

packages :: ConfigT [Name]
packages = getPackages <$> asks config

version :: ConfigT Version
version = getVersion <$> asks config

withConfig :: (Config -> t -> t) -> t -> ConfigT t
withConfig f t = flip f t <$> asks config
