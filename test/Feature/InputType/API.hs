{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Feature.InputType.API
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLRootResolver (..),
    GQLType (..),
    ResolverQ,
    Undefined (..),
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data F1Args = F1Args
  { arg1 :: Text,
    arg2 :: Maybe Int
  }
  deriving (Generic, GQLType)

data F2Args = F2Args
  { argList :: [Text],
    argNestedList :: [Maybe [[Int]]]
  }
  deriving (Generic, GQLType)

data A = A
  { a1 :: F1Args -> ResolverQ () IO Text,
    a2 :: F2Args -> ResolverQ () IO Int
  }
  deriving (Generic, GQLType)

newtype Query (m :: * -> *) = Query
  { q1 :: A
  }
  deriving (Generic, GQLType)

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver =
        Query
          { q1 =
              A
                { a1 = const $ return "a1Test",
                  a2 = const $ return 1
                }
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
