{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
    ServerDec,
    ServerDecContext (..),
    ServerConsD,
    ServerFieldDefinition (..),
    toServerField,
    GQLTypeDefinition (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( ArgumentDefinition,
    ConsD (..),
    Description,
    Directives,
    FieldDefinition (..),
    FieldName,
    TypeKind,
    TypeName,
    TypeRef,
    Value,
  )
import Language.Haskell.TH (Name, Q)
import Relude

data ServerFieldDefinition = ServerFieldDefinition
  { isParametrized :: Bool,
    argumentsTypeName :: Maybe TypeName,
    fieldType :: TypeRef,
    fieldName :: FieldName
  }
  deriving (Show)

toServerField :: FieldDefinition c s -> ServerFieldDefinition
toServerField FieldDefinition {fieldType, fieldName} =
  ServerFieldDefinition
    { isParametrized = False,
      argumentsTypeName = Nothing,
      fieldType,
      fieldName
    }

type ServerConsD = ConsD ServerFieldDefinition

data GQLTypeDefinition s = GQLTypeDefinition
  { gqlKind :: Name,
    gqlTypeDescription :: Maybe Text,
    gqlTypeDescriptions :: Map Text Description,
    gqlTypeDirectives :: Map Text (Directives s),
    gqlTypeDefaultValues :: Map Text (Value s)
  }
  deriving (Show)

--- Core
data ServerTypeDefinition s
  = ServerTypeDefinition
      { -- Type Declaration
        tName :: TypeName,
        typeParameters :: [Name],
        tCons :: [ConsD ServerFieldDefinition],
        tKind :: TypeKind,
        -- GQLType methods
        gql :: Maybe (GQLTypeDefinition s)
      }
  | ServerInterfaceDefinition TypeName TypeName TypeName
  deriving (Show)

type ServerDec = ReaderT ServerDecContext Q

newtype ServerDecContext = ServerDecContext
  { namespace :: Bool
  }
