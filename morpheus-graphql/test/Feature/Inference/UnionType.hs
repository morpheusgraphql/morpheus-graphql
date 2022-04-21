{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Inference.UnionType
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLType (..),
    ResolverQ,
    RootResolver (..),
    Undefined,
    defaultRootResolver,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data A = A
  { aText :: Text,
    aInt :: Int
  }
  deriving (Generic, GQLType)

data B = B
  { bText :: Text,
    bInt :: Int
  }
  deriving (Generic, GQLType)

data C = C
  { cText :: Text,
    cInt :: Int
  }
  deriving (Generic, GQLType)

data Sum
  = SumA A
  | SumB B
  deriving (Generic, GQLType)

data Query m = Query
  { union :: m [Sum],
    fc :: C
  }
  deriving (Generic, GQLType)

resolveUnion :: ResolverQ () IO [Sum]
resolveUnion =
  return [SumA A {aText = "at", aInt = 1}, SumB B {bText = "bt", bInt = 2}]

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver =
        Query
          { union = resolveUnion,
            fc = C {cText = "", cInt = 3}
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
