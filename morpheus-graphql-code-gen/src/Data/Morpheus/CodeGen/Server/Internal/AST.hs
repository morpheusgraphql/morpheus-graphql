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
    CodeGenTypeName (typeParameters),
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    TypeClassInstance (..),
    TypeValue (..),
    printTHName,
  )
import Data.Morpheus.CodeGen.Printer
  ( Printer (..),
    ignore,
    optional,
    unpack,
    (.<>),
  )
import Data.Morpheus.CodeGen.TH
  ( PrintExp (..),
    ToName (..),
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
import Language.Haskell.TH.Lib (appE, varE)
import Prettyprinter
  ( Doc,
    Pretty (..),
    align,
    indent,
    line,
    pretty,
    punctuate,
    tupled,
    vsep,
    (<+>),
  )
import Relude hiding (Show, optional, print, show)
import Prelude (Show (..))

data Kind
  = Scalar
  | Type
  deriving (Show, Eq)

instance Pretty Kind where
  pretty Type = "TYPE"
  pretty Scalar = "SCALAR"

instance ToName Kind where
  toName Scalar = ''SCALAR
  toName Type = ''TYPE

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
  -- TODO: on scalar we should render user provided type
  pretty ScalarType {..} = "type" <+> ignore (print scalarTypeName) <+> "= Int"
  pretty (DataType cgType) = pretty cgType
  pretty (GQLTypeInstance kind gql)
    | kind == Scalar = ""
    | otherwise = renderGQLType gql
  pretty (GQLDirectiveInstance _) = "TODO: not supported"

renderTypeableConstraints :: [Text] -> Doc n
renderTypeableConstraints xs = tupled (map (("Typeable" <+>) . pretty) xs) <+> "=>"

renderGQLType :: TypeClassInstance ServerMethod -> Doc ann
renderGQLType TypeClassInstance {..} =
  "instance"
    <> optional renderTypeableConstraints (typeParameters typeClassTarget)
    <+> printTHName typeClassName
    <+> typeHead
    <+> "where"
      <> line
      <> indent 2 (vsep (map renderAssoc assoc <> map renderMethodD typeClassMethods))
  where
    typeHead = unpack (print typeClassTarget)
    renderAssoc (name, a) = "type" <+> printTHName name <+> typeHead <+> "=" <+> pretty a
    renderMethodD (name, _, method) = printTHName name <+> " _ =" <+> pretty method

newtype CodeGenConfig = CodeGenConfig {namespace :: Bool}

data ServerMethod
  = ServerMethodDefaultValues (Map Text (Value CONST))
  | ServerMethodDirectives [ServerDirectiveUsage]
  deriving (Show)

instance Pretty ServerMethod where
  pretty (ServerMethodDefaultValues x) = pretty (show x)
  pretty (ServerMethodDirectives dirs) = align $ vsep $ punctuate " <>" (map pretty dirs)

instance PrintExp ServerMethod where
  printExp (ServerMethodDefaultValues values) = [|values|]
  printExp (ServerMethodDirectives dirs) = foldr (appE . appE [|(<>)|] . printExp) [|mempty|] dirs
