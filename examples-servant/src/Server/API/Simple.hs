{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Server.API.Simple
  ( simpleApi,
  )
where

import Data.Morpheus
  ( interpreter,
  )
import Data.Morpheus.Document
  ( importGQLDocument,
  )
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    RootResolver (..),
    Undefined (..),
  )
import Data.Text (Text)

importGQLDocument "src/Server/API/simple.gql"

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    deity DeityArgs {name} =
      pure
        Deity
          { name = pure name,
            power = pure (Just "Shapeshifting")
          }

simpleApi :: GQLRequest -> IO GQLResponse
simpleApi = interpreter rootResolver
