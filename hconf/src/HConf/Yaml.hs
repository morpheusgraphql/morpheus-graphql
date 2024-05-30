{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Yaml
  ( readYaml,
    writeYaml,
    rewriteYaml,
    run,
    IOAction (..),
    runSilent,
  )
where

import Control.Exception (tryJust)
import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
  )
import Data.Aeson.KeyMap
import qualified Data.ByteString as L
  ( readFile,
    writeFile,
  )
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import HConf.Config.Config (Config, checkConfig)
import HConf.Config.ConfigT (ConfigT (..), HCEnv (..), runConfigT)
import HConf.Core.Env (Env (..))
import HConf.Utils.Core (compareFields)
import HConf.Utils.Log (alert, info, label, logFileChange, task)
import Relude hiding (Show, Undefined, intercalate, show)
import Prelude (Show (..))

parseYaml :: (FromJSON a) => ByteString -> IO a
parseYaml = decodeThrow

serializeYaml :: (ToJSON a) => a -> ByteString
serializeYaml =
  encodePretty
    $ setConfDropNull True
    $ setConfCompare compareFields defConfig

runSilent :: ConfigT (Maybe Config) -> Env -> IO ()
runSilent t env@Env {..} = do
  cfg <- L.readFile hconf >>= parseYaml
  res <- runConfigT t env cfg
  case res of
    Left x -> alert ("ERROR: " <> x)
    Right _ -> pure ()

run :: String -> ConfigT (Maybe Config) -> Env -> IO ()
run name t env@Env {..} = do
  cfg <- L.readFile hconf >>= parseYaml
  checkConfig cfg
  res <- runConfigT (label name (t >>= save)) env cfg
  case res of
    Left x -> alert ("ERROR: " <> x)
    Right _ -> info "OK"

save :: Maybe Config -> ConfigT ()
save Nothing = pure ()
save (Just cfg) = label "hconf" $ task "hconf.yaml" $ do
  ctx <- asks id
  writeYaml (hconf $ env ctx) cfg

readYaml :: (FromJSON a) => FilePath -> ConfigT a
readYaml = liftIO . (L.readFile >=> parseYaml)

writeYaml :: (ToJSON a) => FilePath -> a -> ConfigT ()
writeYaml path v = checkAndWrite path (serializeYaml v) >>= logFileChange path

checkAndWrite :: (IOAction m) => FilePath -> ByteString -> m Bool
checkAndWrite path newFile = do
  file <- eitherRead path
  write path newFile
  return (fromRight "" file == newFile)

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

mapYamlM :: (Functor m) => (t -> m t) -> Yaml t -> m (Yaml t)
mapYamlM f (Yaml v props) = (`Yaml` props) <$> f v

rewriteYaml :: (FromJSON t, ToJSON t) => FilePath -> (t -> ConfigT t) -> ConfigT ()
rewriteYaml path f = do
  readYaml path
    >>= mapYamlM f
    >>= writeYaml path

class (Monad m) => IOAction m where
  eitherRead :: FilePath -> m (Either String ByteString)
  read :: FilePath -> m ByteString
  write :: FilePath -> ByteString -> m ()

printException :: SomeException -> String
printException = show

instance IOAction IO where
  eitherRead path = tryJust (Just . printException) (L.readFile path)
  read = L.readFile
  write = L.writeFile

instance IOAction ConfigT where
  eitherRead = liftIO . eitherRead
  read = liftIO . read
  write f = liftIO . write f
