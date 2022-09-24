{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Visitors.Object
  ( api,
  )
where

import Data.Kind (Type)
import Data.Morpheus (interpreter)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)

newtype MythologyCity = City
  { cityName :: Text
  }
  deriving (Generic)

instance GQLType MythologyCity where
  directiveUsages _ =
    [ DirectiveUsage
        DirectivePrefix
          { prefix = "Mythology",
            drop = True
          }
    ]

newtype MythologyDeity = MythologyDeity
  { deityName :: Text
  }
  deriving (Generic)

instance GQLType MythologyDeity where
  directiveUsages _ = []

data Query (m :: Type -> Type) = Query
  { deity :: MythologyDeity,
    city :: MythologyCity
  }
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root =
  defaultRootResolver
    { queryResolver =
        Query
          { deity = MythologyDeity "morpheus",
            city = City "corinth"
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter root
