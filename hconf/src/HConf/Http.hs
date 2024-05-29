{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Http
  ( fetchVersions,
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Decoding (eitherDecode)
import Data.Map (lookup)
import qualified Data.Text as T
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

fetchVersionResponse :: (MonadIO m, MonadFail m) => m (Either String (Map Text [String]))
fetchVersionResponse = parseURI "https://hackage.haskell.org/package/morpheus-graphql/preferred.json" >>= httpRequest

lookupVersions :: (MonadFail m) => Either String (Map Text [String]) -> m [String]
lookupVersions (Right x) = maybe (fail "field normal-version not found") pure (lookup "normal-version" x)
lookupVersions (Left x) = fail x

fetchVersions :: (MonadFail m, MonadIO m) => m [String]
fetchVersions = fetchVersionResponse >>= lookupVersions