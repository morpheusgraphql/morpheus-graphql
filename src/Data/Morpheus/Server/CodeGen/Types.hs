{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.CodeGen.Types
  ( ServerTypeDefinition (..),
    ServerDec,
    ServerDecContext (..),
    ServerFieldDefinition (..),
    GQLTypeDefinition (..),
    ServerConstructorDefinition (..),
    FIELD_TYPE_WRAPPER (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( Description,
    Directives,
    FieldName,
    TypeKind,
    TypeName,
    TypeRef,
    Value,
  )
import Language.Haskell.TH (Name, Q)
import Relude

data FIELD_TYPE_WRAPPER = MONAD | SUBSCRIPTION
  deriving (Show)

data ServerFieldDefinition = ServerFieldDefinition
  { isParametrized :: Bool,
    argumentsTypeName :: Maybe TypeName,
    fieldType :: TypeRef,
    fieldName :: FieldName,
    wrappers :: [FIELD_TYPE_WRAPPER]
  }
  deriving (Show)

data GQLTypeDefinition s = GQLTypeDefinition
  { gqlKind :: Name,
    gqlTypeDescription :: Maybe Text,
    gqlTypeDescriptions :: Map Text Description,
    gqlTypeDirectives :: Map Text (Directives s),
    gqlTypeDefaultValues :: Map Text (Value s)
  }
  deriving (Show)

data ServerConstructorDefinition = ServerConstructorDefinition
  { constructorName :: TypeName,
    constructorFields :: [ServerFieldDefinition]
  }
  deriving (Show)

--- Core
data ServerTypeDefinition s
  = ServerTypeDefinition
      { -- Type Declaration
        tName :: TypeName,
        typeParameters :: [Name],
        tCons :: [ServerConstructorDefinition],
        tKind :: TypeKind,
        derives :: [Name],
        -- GQLType methods
        gql :: Maybe (GQLTypeDefinition s)
      }
  | ServerInterfaceDefinition TypeName TypeName TypeName
  deriving (Show)

type ServerDec = ReaderT ServerDecContext Q

newtype ServerDecContext = ServerDecContext
  { namespace :: Bool
  }
