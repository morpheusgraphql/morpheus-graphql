{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
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
    Kind (..),
    ServerDirectiveUsage (..),
    TypeValue (..),
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenField (..),
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    TypeValue (..),
  )
import Data.Morpheus.CodeGen.TH
  ( PrintExp (..),
    PrintType (..),
    apply,
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
import Prettyprinter (Pretty (..))
import Relude

data ModuleDefinition = ModuleDefinition
  { moduleName :: Text,
    imports :: [(Text, [Text])],
    extensions :: [Text],
    types :: [ServerTypeDefinition]
  }

data Kind = Scalar | Type deriving (Show)

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

data GQLTypeDefinition = GQLTypeDefinition
  { gqlKind :: Kind,
    gqlTypeDirectiveUses :: [ServerDirectiveUsage],
    gqlTypeDefaultValues :: Map Text (Value CONST),
    dropNamespace :: Maybe (TypeKind, Text)
  }
  deriving (Show)

data ServerConstructorDefinition = ServerConstructorDefinition
  { constructorName :: TypeName,
    constructorFields :: [CodeGenField]
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
