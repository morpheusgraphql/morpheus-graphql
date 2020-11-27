{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.TypeCategoryCollision.Fail.API
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

data Deity = Deity
  { name :: Text,
    age :: Int
  }
  deriving (Show, Generic, GQLType)

newtype DeityArgs = DeityArgs
  { input :: Deity
  }
  deriving (Show, Generic, GQLType)

newtype Query (m :: * -> *) = Query
  { deity :: DeityArgs -> m Deity
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { deity =
              const $
                pure
                  Deity
                    { name =
                        "Morpheus",
                      age = 1000
                    }
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
