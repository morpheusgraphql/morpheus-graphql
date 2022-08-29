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
  ( FetchError,
    Request (..),
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

fetchFilms :: IO (Either (FetchError StarWarsFilms) StarWarsFilms)
fetchFilms =
  request $
    HttpRequest
      { requestArgs = (),
        httpEndpoint = "https://swapi.graph.cool"
      }
