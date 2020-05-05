{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Feature.UnionType.API
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLRootResolver (..),
    GQLType (..),
    IORes,
    Undefined (..),
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
  { union :: () -> m [Sum],
    fc :: C
  }
  deriving (Generic, GQLType)

resolveUnion :: () -> IORes () [Sum]
resolveUnion _ =
  return [SumA A {aText = "at", aInt = 1}, SumB B {bText = "bt", bInt = 2}]

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver =
        Query
          { union = resolveUnion,
            fc = C {cText = "", cInt = 3}
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
