{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Client.StarWarsClient
  ( fetchFilms,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client
  ( Fetch (..),
    FetchError,
    declareLocalTypesInline,
    raw,
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

declareLocalTypesInline
  "assets/starwars.graphql"
  [raw|
    query StarWarsFilms {
      allFilms {
        title
        characters {
          name
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

fetchFilms :: IO (Either (FetchError StarWarsFilms) StarWarsFilms)
fetchFilms = fetch resolver ()
