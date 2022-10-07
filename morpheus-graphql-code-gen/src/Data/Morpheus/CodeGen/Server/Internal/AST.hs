{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Internal.AST
  ( ModuleDefinition (..),
    CodeGenConfig (..),
    ServerTypeDefinition (..),
    GQLTypeDefinition (..),
    CONST,
    TypeKind (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    unpackName,
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    ServerConstructorDefinition (..),
    ServerFieldDefinition (..),
    Kind (..),
    ServerDirectiveUsage (..),
    TypeValue (..),
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( DerivingClass (..),
    TypeValue (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DirectiveLocation (..),
    FieldName,
    TypeKind (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    Value,
    unpackName,
  )
import Relude

data ModuleDefinition = ModuleDefinition
  { moduleName :: Text,
    imports :: [(Text, [Text])],
    extensions :: [Text],
    types :: [ServerTypeDefinition]
  }

data FIELD_TYPE_WRAPPER
  = MONAD
  | SUBSCRIPTION
  | PARAMETRIZED
  | ARG TypeName
  | TAGGED_ARG FieldName TypeRef
  | GQL_WRAPPER TypeWrapper
  deriving (Show)

data ServerFieldDefinition = ServerFieldDefinition
  { fieldType :: Text,
    fieldName :: FieldName,
    wrappers :: [FIELD_TYPE_WRAPPER]
  }
  deriving (Show)

data Kind = Scalar | Type deriving (Show)

data ServerDirectiveUsage
  = TypeDirectiveUsage TypeValue
  | FieldDirectiveUsage FieldName TypeValue
  | EnumDirectiveUsage TypeName TypeValue
  deriving (Show)

data GQLTypeDefinition = GQLTypeDefinition
  { gqlKind :: Kind,
    gqlTypeDirectiveUses :: [ServerDirectiveUsage],
    gqlTypeDefaultValues :: Map Text (Value CONST)
  }
  deriving (Show)

data ServerConstructorDefinition = ServerConstructorDefinition
  { constructorName :: TypeName,
    constructorFields :: [ServerFieldDefinition]
  }
  deriving (Show)

data ServerTypeDefinition
  = ServerTypeDefinition
      { tName :: Text,
        typeParameters :: [Text],
        tCons :: [ServerConstructorDefinition],
        tKind :: TypeKind,
        derives :: [DerivingClass],
        typeGQLType :: Maybe GQLTypeDefinition
      }
  | DirectiveTypeDefinition
      { directiveConstructor :: ServerConstructorDefinition,
        directiveDerives :: [DerivingClass],
        directiveLocations :: [DirectiveLocation],
        directiveGQLType :: GQLTypeDefinition
      }
  | ServerInterfaceDefinition
      TypeName
      TypeName
      TypeName
  deriving (Show)

newtype CodeGenConfig = CodeGenConfig
  { namespace :: Bool
  }
