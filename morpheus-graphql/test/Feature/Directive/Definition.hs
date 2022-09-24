{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Directive.Definition
  ( api,
  )
where

import Data.Kind (Type)
import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( DirectiveUsage (..),
    GQLDirective (..),
    GQLRequest,
    GQLResponse,
    GQLType (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation (..),
  )
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Deity = MythologyDeity
  { deityName :: Text
  }
  deriving (Generic)

data Power = Power
  { name :: Text,
    isLimited :: Bool
  }
  deriving (GQLType, Generic)

instance GQLDirective Power where
  type ALLOWED_DIRECTIVE_LOCATIONS Power = '[ 'OBJECT]

instance GQLType Deity where
  directiveUsages _ =
    [ DirectiveUsage
        Power
          { name = "Lightning bolts",
            isLimited = False
          }
    ]

newtype Query (m :: Type -> Type) = Query
  {deity :: Deity}
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root = defaultRootResolver {queryResolver = Query {deity = MythologyDeity "morpheus"}}

api :: GQLRequest -> IO GQLResponse
api = interpreter root
