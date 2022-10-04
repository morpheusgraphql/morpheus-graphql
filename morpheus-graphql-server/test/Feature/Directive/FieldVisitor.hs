{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Directive.FieldVisitor
  ( api,
  )
where

import Data.Kind (Type)
import Data.Morpheus.Server (interpreter)
import Data.Morpheus.Server.Types
  ( Describe (..),
    GQLRequest,
    GQLResponse,
    GQLType (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
    fieldDirective,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Deity = Deity
  { name :: Text,
    power :: Maybe Text
  }
  deriving (Generic)

instance GQLType Deity where
  directives _ =
    fieldDirective "name" Describe {text = "name of the deity"}
      <> fieldDirective "power" Describe {text = "extraterrestrial ability"}

newtype Query (m :: Type -> Type) = Query {deity :: Deity}
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root =
  defaultRootResolver
    { queryResolver =
        Query
          { deity =
              Deity
                { name = "morpheus",
                  power = Nothing
                }
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter root
