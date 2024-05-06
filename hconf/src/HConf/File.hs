{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.File
  ( readYaml,
    writeYaml,
    Yaml (..),
    maybeList,
    aesonYAMLOptions,
    mapYaml,
  )
where

import Config.Types
import Data.Aeson (FromJSON (parseJSON), Object, Options (..), ToJSON (toJSON), Value (..), defaultOptions)
import Data.Aeson.KeyMap
import qualified Data.ByteString as L
  ( readFile,
    writeFile,
  )
import Data.Char (isUpper, toLower)
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
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

maybeList :: Maybe [a] -> [a]
maybeList = fromMaybe []

toKebabCase :: String -> String
toKebabCase = concatMap toKebab
  where
    toKebab
      x
        | isUpper x = ['-', (toLower x)]
        | otherwise = [x]

aesonYAMLOptions :: Options
aesonYAMLOptions = defaultOptions {fieldLabelModifier = toKebabCase}

mapYaml :: (t -> t) -> Yaml t -> Yaml t
mapYaml f (Yaml v props) = Yaml (f v) props