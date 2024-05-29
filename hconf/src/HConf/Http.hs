{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Http
  ( httpRequest,
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Decoding (eitherDecode)
import Network.HTTP.Req
  ( GET (..),
    NoReqBody (..),
    Url,
    defaultHttpConfig,
    lbsResponse,
    req,
    responseBody,
    runReq,
  )
import Relude hiding (ByteString)

httpRequest :: (FromJSON a) => Url s -> IO (Either String a)
httpRequest uri = eitherDecode . responseBody <$> runReq defaultHttpConfig (req GET uri NoReqBody lbsResponse mempty)
