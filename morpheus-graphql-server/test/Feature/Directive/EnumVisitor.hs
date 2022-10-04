{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Directive.EnumVisitor
  ( api,
  )
where

import Data.Kind (Type)
import Data.Morpheus.Server (interpreter)
import Data.Morpheus.Server.Types
  ( GQLDirective (..),
    GQLRequest,
    GQLResponse,
    GQLType (..),
    RootResolver (..),
    Undefined,
    VisitEnum (..),
    defaultRootResolver,
    enumDirective,
  )
import Data.Morpheus.Types.Internal.AST (DirectiveLocation (..))
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Rename = Rename {name :: Text}
  deriving
    ( GQLType,
      Generic
    )

instance GQLDirective Rename where
  type DIRECTIVE_LOCATIONS Rename = '[ 'ENUM_VALUE]

instance VisitEnum Rename

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
    enumDirective "Sparta" Rename {name = "sparta"}
      <> enumDirective "Delphi" Rename {name = "delphi"}
      <> enumDirective "Argos" Rename {name = "argos"}

newtype Query (m :: Type -> Type) = Query {city :: City}
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root = defaultRootResolver {queryResolver = Query {city = Corinth}}

api :: GQLRequest -> IO GQLResponse
api = interpreter root
