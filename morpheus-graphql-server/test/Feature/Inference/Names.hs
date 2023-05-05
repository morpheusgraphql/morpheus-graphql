{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Feature.Inference.Names
  ( api,
  )
where

import Data.Morpheus.Server (interpreter)
import Data.Morpheus.Server.Types
  ( Arg (..),
    GQLRequest,
    GQLResponse,
    GQLType (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
  )
import Relude hiding (Undefined)

data MyEnum = MyEnum deriving (Generic, GQLType)

newtype Input = Input {data' :: Text}
  deriving (Generic)
  deriving anyclass (GQLType)

newtype Query (m :: Type -> Type) = Query
  { type' ::
      Arg "in" Input ->
      Text
  }
  deriving (Generic)
  deriving anyclass (GQLType)

root :: RootResolver IO () Query Undefined Undefined
root =
  defaultRootResolver
    { queryResolver = Query {type' = \(Arg Input {data'}) -> data'}
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter root
