{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Http
  ( getLatestBound,
    fetchVersions,
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Decoding (eitherDecode)
import Data.Map (lookup)
import Data.Text (unpack)
import qualified Data.Text as T
import HConf.Bounds (Bound (..), Restriction (..))
import HConf.Utils (Name)
import HConf.Version (Version)
import Network.HTTP.Req
  ( GET (..),
    NoReqBody (..),
    defaultHttpConfig,
    lbsResponse,
    req,
    responseBody,
    runReq,
    useURI,
  )
import Relude hiding (ByteString)
import Text.URI (URI, mkURI)

httpRequest :: (FromJSON a, MonadIO m, MonadFail m) => URI -> m (Either String a)
httpRequest uri = case useURI uri of
  Nothing -> fail ("Invalid Endpoint: " <> show uri <> "!")
  (Just (Left (u, o))) -> liftIO (eitherDecode . responseBody <$> runReq defaultHttpConfig (req GET u NoReqBody lbsResponse o))
  (Just (Right (u, o))) -> liftIO (eitherDecode . responseBody <$> runReq defaultHttpConfig (req GET u NoReqBody lbsResponse o))

parseURI :: (MonadFail m) => String -> m URI
parseURI url = maybe (fail ("Invalid Endpoint: " <> show url <> "!")) pure (mkURI (T.pack url))

fetchVersionResponse :: (MonadIO m, MonadFail m) => String -> m (Either String (Map Text (NonEmpty Version)))
fetchVersionResponse name = parseURI ("https://hackage.haskell.org/package/" <> name <> "/preferred.json") >>= httpRequest

lookupVersions :: (MonadFail m) => Either String (Map Text (NonEmpty Version)) -> m (NonEmpty Version)
lookupVersions (Right x) = maybe (fail "field normal-version not found") pure (lookup "normal-version" x)
lookupVersions (Left x) = fail x

fetchVersions :: (MonadFail m, MonadIO m) => String -> m (NonEmpty Version)
fetchVersions name = fetchVersionResponse name >>= lookupVersions

getLatestBound :: (MonadFail m, MonadIO m) => Name -> m Bound
getLatestBound = fmap (Bound Max True . head) . fetchVersions . unpack