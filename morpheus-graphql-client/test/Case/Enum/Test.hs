{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Enum.Test
  ( test,
  )
where

import Data.Morpheus.Client
  ( declareGlobalTypes,
    declareLocalTypesInline,
    raw,
  )
import Relude
import Spec.Utils
  ( assertFetch,
    path,
  )
import Test.Tasty
  ( TestTree,
  )

declareGlobalTypes (path "Enum/schema.gql")

declareLocalTypesInline
  (path "Enum/schema.gql")
  [raw|
    query MyQuery( $inputCity: City!) {
      city(city:$inputCity)
      cities
    }
  |]

test :: TestTree
test =
  assertFetch
    "Enum"
    Nothing
    MyQueryArgs {inputCity = CityAthens}
    ( Right
        MyQuery
          { city = CityAthens,
            cities =
              [ CityAthens,
                CitySparta,
                CityCorinth,
                CityDelphi,
                CityArgos
              ]
          }
    )
