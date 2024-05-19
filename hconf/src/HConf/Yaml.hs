{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Yaml
  ( readYaml,
    writeYaml,
    aesonYAMLOptions,
    rewriteYaml,
    open,
    save,
  )
where

import Control.Monad.Error.Class (MonadError (catchError))
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Aeson (FromJSON (parseJSON), Object, Options (..), ToJSON (toJSON), Value (..), defaultOptions)
import Data.Aeson.KeyMap
import qualified Data.ByteString as L
  ( readFile,
    writeFile,
  )
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import HConf.ConfigT (ConfigT (..), HCEnv (..), runConfigT)
import HConf.Env (Env (..))
import HConf.Log (logFileChange)
import HConf.Utils (compareFields, toKebabCase)
import Relude hiding (Show, Undefined, intercalate, show)
import Prelude (Show (..))

parseYaml :: (FromJSON a) => ByteString -> IO a
parseYaml = decodeThrow

serializeYaml :: (ToJSON a) => a -> ByteString
serializeYaml = encodePretty (setConfDropNull True $ setConfCompare compareFields defConfig)

open :: ConfigT () -> Env -> IO ()
open t env@Env {..} = do
  cfg <- L.readFile hconf >>= parseYaml
  runConfigT t env cfg

save :: ConfigT ()
save = do
  ctx <- asks id
  writeYaml (hconf $ env ctx) (config ctx)

readYaml :: (FromJSON a) => FilePath -> ConfigT a
readYaml = liftIO . (L.readFile >=> parseYaml)

writeYaml :: (ToJSON a) => FilePath -> a -> ConfigT ()
writeYaml path v = withRunInIO (const $ checkAndWrite path $ serializeYaml v) >>= logFileChange path

checkAndWrite :: FilePath -> ByteString -> IO Bool
checkAndWrite path newFile = do
  file <- catchError (L.readFile path) (const $ pure "")
  L.writeFile path newFile
  return (file == newFile)

data Yaml t = Yaml
  { getData :: t,
    rawValue :: KeyMap Value
  }
  deriving (Generic)

instance (Show t) => Show (Yaml t) where
  show (Yaml t _) = show t

instance (FromJSON t) => FromJSON (Yaml t) where
  parseJSON v = Yaml <$> parseJSON v <*> parseJSON v

instance (ToJSON t) => ToJSON (Yaml t) where
  toJSON (Yaml t v) = Object (toObject (toJSON t) <> v)

toObject :: Value -> Object
toObject (Object x) = x
toObject _ = mempty

aesonYAMLOptions :: Options
aesonYAMLOptions = defaultOptions {fieldLabelModifier = toKebabCase}

mapYamlM :: (Functor m) => (t -> m t) -> Yaml t -> m (Yaml t)
mapYamlM f (Yaml v props) = (`Yaml` props) <$> f v

rewriteYaml :: (FromJSON t, ToJSON t) => FilePath -> (t -> ConfigT t) -> ConfigT ()
rewriteYaml path f = do
  readYaml path
    >>= mapYamlM f
    >>= writeYaml path