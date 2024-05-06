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
  )
where

import Data.Aeson (FromJSON (parseJSON), Object, Options (..), ToJSON (toJSON), Value (..), defaultOptions)
import Data.Aeson.KeyMap
import qualified Data.ByteString as L
  ( readFile,
    writeFile,
  )
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import HConf.Utils (compareFields, toKebabCase)
import Relude hiding (Show, Undefined, intercalate, show)
import Prelude (Show (..))

parseYaml :: (FromJSON a) => ByteString -> IO a
parseYaml = decodeThrow

serializeYaml :: (ToJSON a) => a -> ByteString
serializeYaml = encodePretty (setConfDropNull True $ setConfCompare compareFields defConfig)

readYaml :: (FromJSON a) => FilePath -> IO a
readYaml = L.readFile >=> parseYaml

writeYaml :: (ToJSON a) => FilePath -> a -> IO ()
writeYaml path = L.writeFile path . serializeYaml

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

rewriteYaml :: (FromJSON t, ToJSON t) => FilePath -> (t -> IO t) -> IO ()
rewriteYaml path f =
  readYaml path
    >>= mapYamlM f
    >>= writeYaml path