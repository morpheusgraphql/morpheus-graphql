{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
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
    ServerMethod (..),
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenType,
    CodeGenTypeName,
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    TypeClassInstance (..),
    TypeValue (..),
  )
import Data.Morpheus.CodeGen.Printer
  ( Printer (..),
    ignore,
    unpack,
    (.<>),
  )
import Data.Morpheus.CodeGen.TH (PrintDec (..), PrintExp (..), ToName (..), apply, m', m_, printTypeSynonym)
import Data.Morpheus.Server.Types (DIRECTIVE, SCALAR, TYPE, TypeGuard, enumDirective, fieldDirective, typeDirective)
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
import Language.Haskell.TH.Lib (appE, varE)
import Prettyprinter
  ( Pretty (..),
    align,
    concatWith,
    indent,
    line,
    pretty,
    (<+>),
  )
import Relude hiding (Show, optional, print, show)
import Prelude (Show (..))

data Kind
  = Scalar
  | Type
  | Directive
  deriving (Show, Eq)

instance Pretty Kind where
  pretty Type = "TYPE"
  pretty Scalar = "SCALAR"
  pretty Directive = "DIRECTIVE"

instance ToName Kind where
  toName Scalar = ''SCALAR
  toName Type = ''TYPE
  toName Directive = ''DIRECTIVE

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
    gqlTypeDirectiveUses :: [ServerDirectiveUsage]
  }
  deriving (Show)

data InterfaceDefinition = InterfaceDefinition
  { aliasName :: TypeName,
    interfaceName :: TypeName,
    unionName :: TypeName
  }
  deriving (Show)

instance PrintDec InterfaceDefinition where
  printDec InterfaceDefinition {..} =
    pure
      $ printTypeSynonym
        aliasName
        [m_]
        ( apply
            ''TypeGuard
            [apply interfaceName [m'], apply unionName [m']]
        )

data GQLDirectiveTypeClass = GQLDirectiveTypeClass
  { directiveTypeName :: CodeGenTypeName,
    directiveLocations :: [DirectiveLocation]
  }
  deriving (Show)

data ServerDeclaration
  = GQLTypeInstance Kind (TypeClassInstance ServerMethod)
  | GQLDirectiveInstance (TypeClassInstance ServerMethod)
  | DataType CodeGenType
  | ScalarType {scalarTypeName :: Text}
  | InterfaceType InterfaceDefinition
  deriving (Show)

instance Pretty ServerDeclaration where
  pretty (InterfaceType InterfaceDefinition {..}) =
    "type"
      <+> ignore (print aliasName)
      <+> "m"
      <+> "="
      <+> "TypeGuard"
      <+> unpack (print interfaceName .<> "m")
      <+> unpack (print unionName .<> "m")
      <> line
  pretty ScalarType {} = ""
  pretty (DataType cgType) = pretty cgType
  pretty (GQLTypeInstance kind gql)
    | kind == Scalar = ""
    | otherwise = pretty gql
  pretty (GQLDirectiveInstance _) = ""

newtype CodeGenConfig = CodeGenConfig {namespace :: Bool}

data ServerMethod
  = ServerMethodDefaultValues (Map Text (Value CONST))
  | ServerMethodDirectives [ServerDirectiveUsage]
  deriving (Show)

instance Pretty ServerMethod where
  pretty (ServerMethodDefaultValues x) = pretty (show x)
  pretty (ServerMethodDirectives dirs) = line <> indent 2 (align $ concatWith (\x y -> x <> "\n  <>" <+> y) (map pretty dirs))

instance PrintExp ServerMethod where
  printExp (ServerMethodDefaultValues values) = [|values|]
  printExp (ServerMethodDirectives dirs) = foldr (appE . appE [|(<>)|] . printExp) [|mempty|] dirs
