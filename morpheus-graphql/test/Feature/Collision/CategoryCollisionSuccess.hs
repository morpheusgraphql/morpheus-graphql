{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Collision.CategoryCollisionSuccess
  ( api,
  )
where

import Data.Kind (Type)
import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLType (..),
    GQLTypeOptions (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
  )
import Data.Text
  ( Text,
  )
import GHC.Generics (Generic)

data Deity = Deity
  { name :: Text,
    age :: Int
  }
  deriving (Show, Generic)

nonClashingTypeNameModifier :: Bool -> String -> String
nonClashingTypeNameModifier True original = "Input" ++ original
nonClashingTypeNameModifier False original = original

instance GQLType Deity where
  typeOptions _ opt = opt {typeNameModifier = nonClashingTypeNameModifier}

newtype DeityArgs = DeityArgs
  { input :: Deity
  }
  deriving (Show, Generic, GQLType)

newtype Query (m :: Type -> Type) = Query
  { deity :: DeityArgs -> m Deity
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
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
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
