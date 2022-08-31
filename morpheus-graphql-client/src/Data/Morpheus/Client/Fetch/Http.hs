{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch.Http
  ( post,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Network.HTTP.Req
  ( POST (..),
    ReqBodyLbs (ReqBodyLbs),
    defaultHttpConfig,
    header,
    lbsResponse,
    req,
    responseBody,
    runReq,
    useURI,
  )
import qualified Network.HTTP.Req as R (Option)
import Relude hiding (ByteString)
import Text.URI (URI)

headers :: R.Option scheme
headers = header "Content-Type" "application/json"

post :: URI -> ByteString -> IO ByteString
post uri body = case useURI uri of
  Nothing -> fail ("Invalid Endpoint: " <> show uri <> "!")
  (Just (Left (u, o))) -> responseBody <$> runReq defaultHttpConfig (req POST u (ReqBodyLbs body) lbsResponse (o <> headers))
  (Just (Right (u, o))) -> responseBody <$> runReq defaultHttpConfig (req POST u (ReqBodyLbs body) lbsResponse (o <> headers))
