{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Interface
  ( fetchFilms,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client
  ( Fetch (..),
    defineByDocumentFile,
    gql,
  )
import Data.Text (Text)
import Network.HTTP.Req
  ( POST (..),
    ReqBodyLbs (..),
    defaultHttpConfig,
    header,
    https,
    lbsResponse,
    req,
    responseBody,
    runReq,
  )

defineByDocumentFile
  "morpheus-graphql-examples-client/assets/test-interface.gql"
  [gql|
    query MyQuery {
      myInterface {
        ... on Commit {
              author
        }
      }
    }
  |]

resolver :: ByteString -> IO ByteString
resolver b = runReq defaultHttpConfig $ do
  let headers = header "Content-Type" "application/json"
  responseBody
    <$> req
      POST
      (https "swapi.graph.cool")
      (ReqBodyLbs b)
      lbsResponse
      headers

fetchFilms :: IO (Either String StarWarsFilms)
fetchFilms = fetch resolver ()
