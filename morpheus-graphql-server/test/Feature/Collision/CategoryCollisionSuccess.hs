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
import Data.Morpheus.Server (interpreter)
import Data.Morpheus.Server.Types
  ( GQLRequest,
    GQLResponse,
    GQLType (..),
    InputTypeNamespace (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
    typeDirective,
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

instance GQLType Deity where
  directives _ =
    typeDirective InputTypeNamespace {inputTypeNamespace = "Input"}

newtype DeityArgs = DeityArgs
  { input :: Deity
  }
  deriving
    ( Show,
      Generic,
      GQLType
    )

newtype Query (m :: Type -> Type) = Query
  { deity :: DeityArgs -> m Deity
  }
  deriving
    ( Generic,
      GQLType
    )

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver =
        Query
          { deity =
              const
                $ pure
                  Deity
                    { name =
                        "Morpheus",
                      age = 1000
                    }
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
