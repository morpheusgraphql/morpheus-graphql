{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenType (..),
    CodeGenTypeName (..),
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    TypeValue (..),
    fromTypeName,
    getFullName,
    ModuleDefinition (..),
    TypeClassInstance (..),
    AssociatedType (..),
    MethodArgument (..),
    printTHName,
  )
where

import Data.Morpheus.CodeGen.Internal.Name (camelCaseTypeName)
import Data.Morpheus.CodeGen.Printer
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation,
    FieldName,
    TypeName,
    TypeRef,
    TypeWrapper,
    packName,
    unpackName,
  )
import qualified Language.Haskell.TH.Syntax as TH
import Prettyprinter
  ( Doc,
    Pretty (..),
    comma,
    enclose,
    hsep,
    indent,
    line,
    list,
    nest,
    pretty,
    punctuate,
    tupled,
    vsep,
    (<+>),
  )
import Relude hiding (optional, print)

data DerivingClass
  = SHOW
  | GENERIC
  | CLASS_EQ
  deriving (Show)

instance Pretty DerivingClass where
  pretty SHOW = "Show"
  pretty GENERIC = "Generic"
  pretty CLASS_EQ = "Eq"

data TypeValue
  = TypeValueObject TypeName [(FieldName, TypeValue)]
  | TypeValueNumber Double
  | TypeValueString Text
  | TypeValueBool Bool
  | TypeValueList [TypeValue]
  | TypedValueMaybe (Maybe TypeValue)
  deriving (Show)

renderField :: (FieldName, TypeValue) -> Doc n
renderField (fName, fValue) = pretty (unpackName fName :: Text) <> "=" <+> pretty fValue

instance Pretty TypeValue where
  pretty (TypeValueObject name xs) =
    pretty (unpackName name :: Text)
      <+> "{"
      <+> vsep (punctuate "," (map renderField xs))
      <+> "}"
  pretty (TypeValueNumber x) = pretty x
  pretty (TypeValueString x) = pretty (show x :: String)
  pretty (TypeValueBool x) = pretty x
  pretty (TypedValueMaybe (Just x)) = "Just" <+> pretty x
  pretty (TypedValueMaybe Nothing) = "Nothing"
  pretty (TypeValueList xs) = prettyList xs

data CodeGenType = CodeGenType
  { cgTypeName :: CodeGenTypeName,
    cgConstructors :: [CodeGenConstructor],
    cgDerivations :: [DerivingClass]
  }
  deriving (Show)

isNewType :: CodeGenType -> Bool
isNewType CodeGenType {cgConstructors = [CodeGenConstructor {constructorFields = [_]}]} = True
isNewType _ = False

instance Pretty CodeGenType where
  pretty t@CodeGenType {..} =
    (if isNewType t then "newtype" else "data")
      <+> ignore (print cgTypeName)
        <> renderConstructors cgConstructors
        <> line
        <> indent 2 (renderDeriving cgDerivations)
        <> line
    where
      renderConstructors [cons] = (" =" <+>) $ print' cons
      renderConstructors conses = nest 2 . (line <>) . vsep . prefixVariants $ map print' conses
      prefixVariants (x : xs) = "=" <+> x : map ("|" <+>) xs
      prefixVariants [] = []

renderDeriving :: [DerivingClass] -> Doc n
renderDeriving = ("deriving" <+>) . tupled . map pretty

data CodeGenConstructor = CodeGenConstructor
  { constructorName :: CodeGenTypeName,
    constructorFields :: [CodeGenField]
  }
  deriving (Show)

instance Printer CodeGenConstructor where
  print CodeGenConstructor {constructorFields = [], ..} =
    print constructorName
  print CodeGenConstructor {..} = do
    let fields = map (unpack . print) constructorFields
    pack (print' constructorName <> renderSet fields)
    where
      renderSet = nest 2 . enclose "\n{ " "\n}" . nest 2 . vsep . punctuate comma

data CodeGenField = CodeGenField
  { fieldName :: FieldName,
    fieldType :: TypeName,
    wrappers :: [FIELD_TYPE_WRAPPER],
    fieldIsNullable :: Bool
  }
  deriving (Show)

instance Printer CodeGenField where
  print CodeGenField {..} = infix' (print fieldName) "::" (foldr renderWrapper (print fieldType) wrappers)

data FIELD_TYPE_WRAPPER
  = MONAD
  | SUBSCRIPTION TH.Name
  | PARAMETRIZED
  | ARG TypeName
  | TAGGED_ARG TH.Name FieldName TypeRef
  | GQL_WRAPPER TypeWrapper
  deriving (Show)

renderWrapper :: FIELD_TYPE_WRAPPER -> HSDoc n -> HSDoc n
renderWrapper PARAMETRIZED = (.<> "m")
renderWrapper MONAD = ("m" .<>)
renderWrapper SUBSCRIPTION {} = id
renderWrapper (GQL_WRAPPER typeWrappers) = wrapped typeWrappers
renderWrapper (ARG name) = infix' (print name) "->"
renderWrapper (TAGGED_ARG _ name typeRef) = infix' (apply "Arg" [print (show name :: String), print typeRef]) "->"

data CodeGenTypeName = CodeGenTypeName
  { namespace :: [FieldName],
    typeParameters :: [Text],
    typename :: TypeName
  }
  deriving (Show)

getFullName :: CodeGenTypeName -> TypeName
getFullName CodeGenTypeName {..} = camelCaseTypeName namespace typename

fromTypeName :: TypeName -> CodeGenTypeName
fromTypeName = CodeGenTypeName [] []

instance Printer CodeGenTypeName where
  print cgName =
    HSDoc (not $ null (typeParameters cgName)) $
      parametrizedType
        (unpackName (getFullName cgName))
        (typeParameters cgName)

parametrizedType :: Text -> [Text] -> Doc ann
parametrizedType tName typeParameters = hsep $ map pretty $ tName : typeParameters

data ModuleDefinition dec = ModuleDefinition
  { moduleName :: Text,
    imports :: [(Text, [Text])],
    extensions :: [Text],
    types :: [dec]
  }

instance Pretty dec => Pretty (ModuleDefinition dec) where
  pretty ModuleDefinition {..} =
    vsep
      (map renderExtension extensions)
      <> "{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}"
      <> line
      <> line
      <> "{-# HLINT ignore \"Use camelCase\" #-}"
      <> line
      <> line
      <> "module"
      <+> pretty moduleName
      <+> "where"
        <> line
        <> line
        <> vsep (map renderImport imports)
        <> line
        <> line
        <> vsep (map pretty types)

renderExtension :: Text -> Doc ann
renderExtension name = "{-#" <+> "LANGUAGE" <+> pretty name <+> "#-}"

renderImport :: (Text, [Text]) -> Doc ann
renderImport (src, ls) = "import" <+> pretty src <> renderImportList ls

renderImportList :: [Text] -> Doc ann
renderImportList ["*"] = ""
renderImportList xs = tupled (map pretty xs)

data TypeClassInstance body = TypeClassInstance
  { typeClassName :: TH.Name,
    typeClassContext :: [(TH.Name, TH.Name)],
    typeClassTarget :: CodeGenTypeName,
    assoc :: [(TH.Name, AssociatedType)],
    typeClassMethods :: [(TH.Name, MethodArgument, body)]
  }
  deriving (Show)

instance Pretty a => Pretty (TypeClassInstance a) where
  pretty TypeClassInstance {..} =
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
      renderMethodD (name, args, method) = printTHName name <+> pretty args <+> "=" <+> pretty method

renderTypeableConstraints :: [Text] -> Doc n
renderTypeableConstraints xs = tupled (map (("Typeable" <+>) . pretty) xs) <+> "=>"

data AssociatedType
  = AssociatedTypeName TH.Name
  | AssociatedLocations [DirectiveLocation]
  deriving (Show)

printTHName :: TH.Name -> Doc ann
printTHName = ignore . print . packName

printPromotedLocation :: DirectiveLocation -> Doc ann
printPromotedLocation = ("'" <>) . ignore . print

instance Pretty AssociatedType where
  pretty (AssociatedTypeName x) = printTHName x
  pretty (AssociatedLocations x) = list $ map printPromotedLocation x

data MethodArgument
  = NoArgument
  | ProxyArgument
  | DestructArgument CodeGenConstructor
  deriving (Show)

instance Pretty MethodArgument where
  pretty NoArgument = ""
  pretty ProxyArgument = "_"
  pretty (DestructArgument x) = "{-- TODO: fix me --}"
