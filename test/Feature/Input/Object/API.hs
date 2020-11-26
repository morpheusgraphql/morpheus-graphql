{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Input.Object.API
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLType (..),
    RootResolver (..),
    Undefined (..),
  )
import Data.Text
  ( Text,
    pack,
  )
import GHC.Generics (Generic)

data InputObject = InputObject
  { field :: Text,
    nullableField :: Maybe Int,
    recursive :: Maybe InputObject
  }
  deriving (Generic, Show, GQLType)

-- types & args
newtype Arg a = Arg
  { value :: a
  }
  deriving (Generic, Show, GQLType)

-- query
testRes :: Show a => Applicative m => Arg a -> m Text
testRes Arg {value} = pure $ pack $ show value

-- resolver
newtype Query m = Query
  { input :: Arg InputObject -> m Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {input = testRes},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
