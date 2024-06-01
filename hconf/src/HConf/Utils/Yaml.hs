{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Yaml
  ( readYaml,
    writeYaml,
    rewriteYaml,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
  )
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import HConf.Utils.Class (HConfIO (..))
import HConf.Utils.Core (compareFields)
import HConf.Utils.Log (Log, logFileChange)
import Relude hiding (Show, Undefined, intercalate, show)
import Prelude (Show (..))

serializeYaml :: (ToJSON a) => a -> ByteString
serializeYaml =
  encodePretty $
    setConfDropNull True $
      setConfCompare compareFields defConfig

readYaml :: (FromJSON a, HConfIO m) => FilePath -> m a
readYaml = read >=> (liftIO . decodeThrow)

writeYaml :: (ToJSON a, HConfIO m, Log m) => FilePath -> a -> m ()
writeYaml path v = checkAndWrite path (serializeYaml v) >>= logFileChange path

checkAndWrite :: (HConfIO m) => FilePath -> ByteString -> m Bool
checkAndWrite path newFile = do
  file <- eitherRead path
  write path newFile
  return (fromRight "" file == newFile)

data Yaml t = Yaml
  { getData :: t,
    rawValue :: Object
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

mapYaml :: (Functor m) => (t -> m t) -> Yaml t -> m (Yaml t)
mapYaml f (Yaml v props) = (`Yaml` props) <$> f v

rewriteYaml :: (HConfIO m, Log m, FromJSON t, ToJSON t) => FilePath -> (t -> m t) -> m t
rewriteYaml path f = do
  readYaml path
    >>= mapYaml f
    >>= \x -> writeYaml path x >> pure (getData x)
