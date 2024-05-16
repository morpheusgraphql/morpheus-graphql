{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Yaml
  ( readYaml,
    writeYaml,
    Yaml (..),
    aesonYAMLOptions,
    mapYaml,
    mapYamlM,
    rewriteYaml,
    open,
    save,
  )
where

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
import HConf.Log
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
writeYaml path v = withRunInIO (\_ -> L.writeFile path (serializeYaml v)) >> log ("   updated: " <> withColor darkGray path)

data Yaml t = Yaml
  { getData :: t,
    rawValue :: (KeyMap Value)
  }
  deriving (Generic)

instance (Show t) => Show (Yaml t) where
  show (Yaml t _) = show t

instance (FromJSON t) => FromJSON (Yaml t) where
  parseJSON v = Yaml <$> parseJSON v <*> parseJSON v

instance (ToJSON t) => ToJSON (Yaml t) where
  toJSON (Yaml t v) = do
    let override = toObject (toJSON t)
    (Object (override <> v))

toObject :: Value -> Object
toObject (Object x) = x
toObject _ = mempty

aesonYAMLOptions :: Options
aesonYAMLOptions = defaultOptions {fieldLabelModifier = toKebabCase}

mapYaml :: (t -> t) -> Yaml t -> Yaml t
mapYaml f (Yaml v props) = Yaml (f v) props

mapYamlM :: (Functor m) => (t -> m t) -> Yaml t -> m (Yaml t)
mapYamlM f (Yaml v props) = (`Yaml` props) <$> f v

rewriteYaml :: (FromJSON t, ToJSON t) => FilePath -> (t -> ConfigT t) -> ConfigT ()
rewriteYaml path f =
  readYaml path
    >>= mapYamlM f
    >>= writeYaml path