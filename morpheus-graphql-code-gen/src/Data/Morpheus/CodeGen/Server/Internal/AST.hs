{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Internal.AST
  ( ModuleDefinition (..),
    CodeGenConfig (..),
    ServerDeclaration (..),
    GQLTypeDefinition (..),
    CONST,
    TypeKind (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    unpackName,
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    Kind (..),
    ServerDirectiveUsage (..),
    TypeValue (..),
    InterfaceDefinition (..),
    GQLDirectiveTypeClass (..),
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenType,
    CodeGenTypeName,
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    TypeValue (..),
  )
import Data.Morpheus.CodeGen.TH
  ( PrintExp (..),
    PrintType (..),
  )
import Data.Morpheus.Server.Types
  ( SCALAR,
    TYPE,
    enumDirective,
    fieldDirective,
    typeDirective,
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
import Language.Haskell.TH.Lib (appE, conT, varE)
import Prettyprinter (Pretty (..), (<+>))
import Relude

data ModuleDefinition = ModuleDefinition
  { moduleName :: Text,
    imports :: [(Text, [Text])],
    extensions :: [Text],
    types :: [ServerDeclaration]
  }

data Kind
  = Scalar
  | Type
  deriving (Show, Eq)

instance Pretty Kind where
  pretty Type = "TYPE"
  pretty Scalar = "SCALAR"

instance PrintType Kind where
  printType Scalar = conT ''SCALAR
  printType Type = conT ''TYPE

data ServerDirectiveUsage
  = TypeDirectiveUsage TypeValue
  | FieldDirectiveUsage FieldName TypeValue
  | EnumDirectiveUsage TypeName TypeValue
  deriving (Show)

instance PrintExp ServerDirectiveUsage where
  printExp (TypeDirectiveUsage x) = appE (varE 'typeDirective) (printExp x)
  printExp (FieldDirectiveUsage field x) = appE (appE (varE 'fieldDirective) [|field|]) (printExp x)
  printExp (EnumDirectiveUsage enum x) = appE (appE (varE 'enumDirective) [|enum|]) (printExp x)

instance Pretty ServerDirectiveUsage where
  pretty (TypeDirectiveUsage value) = "typeDirective" <+> pretty value
  pretty (FieldDirectiveUsage place value) = "fieldDirective" <+> pretty (show place :: String) <+> pretty value
  pretty (EnumDirectiveUsage place value) = "enumDirective" <+> pretty (show place :: String) <+> pretty value

data GQLTypeDefinition = GQLTypeDefinition
  { gqlTarget :: CodeGenTypeName,
    gqlKind :: Kind,
    gqlTypeDirectiveUses :: [ServerDirectiveUsage],
    gqlTypeDefaultValues :: Map Text (Value CONST)
  }
  deriving (Show)

data InterfaceDefinition = InterfaceDefinition
  { aliasName :: TypeName,
    interfaceName :: TypeName,
    unionName :: TypeName
  }
  deriving (Show)

data GQLDirectiveTypeClass = GQLDirectiveTypeClass
  { directiveTypeName :: CodeGenTypeName,
    directiveLocations :: [DirectiveLocation]
  }
  deriving (Show)

data ServerDeclaration
  = GQLTypeInstance GQLTypeDefinition
  | GQLDirectiveInstance GQLDirectiveTypeClass
  | DataType CodeGenType
  | ScalarType {scalarTypeName :: Text}
  | InterfaceType InterfaceDefinition
  deriving (Show)

newtype CodeGenConfig = CodeGenConfig
  { namespace :: Bool
  }
