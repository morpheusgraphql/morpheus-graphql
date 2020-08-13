{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  "assets/interface.gql"
  [gql|
    query MyQuery {
      character {
        name
        ... on Deity {
              power
        }
        ... on Hero {

              hoby
        }
      }
      character2: character {
        name1: name
        name
      }
      character3: character {
        ... on Hero {
              hoby
        }
        ... on Character {
              name2: name
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

fetchFilms :: IO (Either String MyQuery)
fetchFilms = fetch resolver ()
