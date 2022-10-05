{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Directive.Definition
  ( api,
  )
where

import Data.Kind (Type)
import Data.Morpheus.Server (interpreter)
import Data.Morpheus.Server.Types
  ( Deprecated (..),
    GQLDirective (..),
    GQLRequest,
    GQLResponse,
    GQLType (..),
    Prefixes (..),
    RootResolver (..),
    Undefined,
    VisitType (..),
    defaultRootResolver,
    enumDirective,
    fieldDirective,
    typeDirective,
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation (..),
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data MythologyDeity = MythologyDeity
  { deityName :: Text,
    deprecatedField :: Maybe Text,
    deprecatedFieldWithReason :: Bool
  }
  deriving (Generic)

data Power = Power
  { name :: Text,
    isLimited :: Bool
  }
  deriving (GQLType, Generic)

instance GQLDirective Power where
  type DIRECTIVE_LOCATIONS Power = '[ 'OBJECT]

instance VisitType Power where
  visitTypeName _ = id

instance GQLType MythologyDeity where
  directives _ =
    typeDirective Power {name = "Lightning bolts", isLimited = False}
      <> typeDirective Prefixes {addPrefix = "", removePrefix = "Mythology"}
      <> fieldDirective 'deprecatedField Deprecated {reason = Nothing}
      <> fieldDirective 'deprecatedFieldWithReason Deprecated {reason = Just "this should be deprecated"}

data City
  = Athens
  | Sparta
  | Corinth
  | Delphi
  | Argos
  deriving
    (Generic)

instance GQLType City where
  directives _ =
    enumDirective 'Sparta Deprecated {reason = Nothing}
      <> enumDirective 'Delphi Deprecated {reason = Just "oracle left the place"}
      <> enumDirective 'Argos Deprecated {reason = Just "for some reason"}

data Query (m :: Type -> Type) = Query
  { deity :: MythologyDeity,
    city :: City
  }
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root =
  defaultRootResolver
    { queryResolver =
        Query
          { deity =
              MythologyDeity
                { deityName = "morpheus",
                  deprecatedField = Nothing,
                  deprecatedFieldWithReason = False
                },
            city = Corinth
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter root
