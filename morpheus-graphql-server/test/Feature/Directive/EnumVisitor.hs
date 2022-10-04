{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
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

newtype Describe = Describe {text :: Text}
  deriving
    ( GQLType,
      Generic
    )

instance GQLDirective Describe where
  type DIRECTIVE_LOCATIONS Describe = '[ 'ENUM_VALUE]

instance VisitEnum Describe where
  visitEnumDescription Describe {text} _ = Just text

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
    enumDirective "Sparta" Describe {text = "city of warriors"}
      <> enumDirective "Delphi" Describe {text = "city of oracle"}
      <> enumDirective "Argos" Describe {text = "city of argonauts"}

newtype Query (m :: Type -> Type) = Query {city :: City}
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root = defaultRootResolver {queryResolver = Query {city = Corinth}}

api :: GQLRequest -> IO GQLResponse
api = interpreter root
