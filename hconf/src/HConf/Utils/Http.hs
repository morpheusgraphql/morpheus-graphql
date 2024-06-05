{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Http
  ( hackage,
  )
where

import Data.Aeson (FromJSON, eitherDecode)
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

hackage :: (MonadIO m, MonadFail m, FromJSON a) => [String] -> m (Either String a)
hackage path = parseURI ("https://hackage.haskell.org/" <> intercalate "/" path <> ".json") >>= httpRequest
