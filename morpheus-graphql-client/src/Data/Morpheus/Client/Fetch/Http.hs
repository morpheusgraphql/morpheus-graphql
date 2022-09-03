{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch.Http
  ( httpRequest,
  )
where

import Data.Aeson
import qualified Data.ByteString.Char8 as L
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map as M
import Data.Morpheus.Client.Fetch.GQLClient
import Data.Morpheus.Client.Fetch.RequestType
import Data.Morpheus.Client.Internal.Types (GQLClientResult)
import qualified Data.Text as T
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

withHeader :: Header -> R.Option scheme
withHeader (k, v) = header (L.pack $ T.unpack k) (L.pack $ T.unpack v)

setHeaders :: Headers -> R.Option scheme
setHeaders = foldMap withHeader . M.toList

post :: URI -> ByteString -> Headers -> IO ByteString
post uri body headers = case useURI uri of
  Nothing -> fail ("Invalid Endpoint: " <> show uri <> "!")
  (Just (Left (u, o))) -> responseBody <$> runReq defaultHttpConfig (req POST u (ReqBodyLbs body) lbsResponse (o <> setHeaders headers))
  (Just (Right (u, o))) -> responseBody <$> runReq defaultHttpConfig (req POST u (ReqBodyLbs body) lbsResponse (o <> setHeaders headers))

httpRequest :: (FromJSON a, RequestType a, ToJSON (RequestArgs a)) => URI -> Request a -> Headers -> IO (GQLClientResult a)
httpRequest uri r h = decodeResponse <$> post uri (encode $ toRequest r) h