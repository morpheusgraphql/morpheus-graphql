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

import Data.Morpheus.Client
  ( GQLClient,
    ResponseStream,
    declareLocalTypesInline,
    raw,
    request,
  )
import Data.Text (Text)

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

fetchFilms :: GQLClient -> IO (ResponseStream StarWarsFilms)
fetchFilms = (`request` ())
