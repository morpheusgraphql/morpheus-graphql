{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Internal.AST
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

import Data.Morpheus.Types.Internal.AST
  ( CONST,
    Description,
    DirectiveLocation (..),
    Directives,
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

data DerivingClass
  = SHOW
  | GENERIC
  | GQL_TYPE
  deriving (Show)

data ServerFieldDefinition = ServerFieldDefinition
  { fieldType :: Text,
    fieldName :: FieldName,
    wrappers :: [FIELD_TYPE_WRAPPER]
  }
  deriving (Show)

data Kind = Scalar | Type deriving (Show)

data TypeValue
  = TypeValueObject TypeName [(FieldName, TypeValue)]
  | TypeValueNumber Double
  | TypeValueString Text
  | TypeValueBool Bool
  | TypeValueList [TypeValue]
  | TypedValueMaybe (Maybe TypeValue)
  deriving (Show)

data ServerDirectiveUsage
  = TypeDirectiveUsage TypeValue
  | FieldDirectiveUsage FieldName TypeValue
  | EnumDirectiveUsage TypeName TypeValue
  deriving (Show)

data GQLTypeDefinition = GQLTypeDefinition
  { gqlKind :: Kind,
    gqlTypeDescription :: Maybe Text,
    gqlTypeDescriptions :: Map Text Description,
    gqlTypeDirectives :: Map Text (Directives CONST),
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
        gql :: Maybe GQLTypeDefinition
      }
  | DirectiveTypeDefinition
      { directiveConstructor :: ServerConstructorDefinition,
        directiveDerives :: [DerivingClass],
        directiveLocations :: [DirectiveLocation]
      }
  | ServerInterfaceDefinition
      TypeName
      TypeName
      TypeName
  deriving (Show)

newtype CodeGenConfig = CodeGenConfig
  { namespace :: Bool
  }
