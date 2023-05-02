{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Feature.Inference.Names
  ( api,
  )
where

import Data.Morpheus.Server (interpreter)
import Data.Morpheus.Server.Resolvers
  ( constRes,
  )
import Data.Morpheus.Server.Types
  ( Arg (..),
    GQLRequest,
    GQLResponse,
    GQLType (..),
    RootResolver (..),
    SubscriptionField,
    Undefined,
    defaultRootResolver,
    subscribe,
  )
import Data.Morpheus.Subscriptions (Event)
import Relude hiding (Undefined)

data MyEnum = MyEnum deriving (Generic, GQLType)

newtype Input = Input {data' :: Text} deriving (Generic, GQLType)

newtype Query (m :: Type -> Type) = Query
  { type' ::
      Arg "input" Input ->
      Text
  }
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root =
  defaultRootResolver
    { queryResolver = Query {type' = \(Arg Input {data'}) -> data'}
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter root