{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Input.DefaultValue.API
  ( api,
    rootResolver,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    ID (..),
    RootResolver (..),
    Undefined (..),
  )
import Data.Text (Text)

importGQLDocument "test/Feature/Input/DefaultValue/schema.gql"

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {user},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    user :: Applicative m => m (User m)
    user =
      pure
        User
          { inputs = const (pure "input")
          }

-----------------------------------
api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
