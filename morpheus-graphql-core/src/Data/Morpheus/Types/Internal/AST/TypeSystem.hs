{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Types.Internal.AST.TypeSystem
  ( Arguments,
    ScalarDefinition (..),
    DataEnum,
    FieldsDefinition,
    ArgumentDefinition,
    DataUnion,
    ArgumentsDefinition (..),
    FieldDefinition (..),
    InputFieldsDefinition,
    TypeContent (..),
    TypeDefinition (..),
    Schema (..),
    DataEnumValue (..),
    TypeLib,
    Directive (..),
    TypeUpdater,
    TypeCategory,
    DataInputUnion,
    Argument (..),
    Fields (..),
    createEnumType,
    createScalarType,
    createType,
    createUnionType,
    createAlias,
    mkInputUnionFields,
    createEnumValue,
    defineType,
    initTypeLib,
    insertType,
    fieldVisibility,
    kindOf,
    toListField,
    isEntNode,
    lookupDeprecated,
    lookupDeprecatedReason,
    lookupWith,
    unsafeFromFields,
    __inputname,
    updateSchema,
    OUT,
    IN,
    ANY,
    FromAny (..),
    ToAny (..),
    DirectiveDefinitions,
    DirectiveDefinition (..),
    Directives,
    fieldsToArguments,
    FieldContent (..),
    fieldContentArgs,
    mkInputValue,
    mkObjectField,
    UnionMember (..),
    mkUnionMember,
    SchemaDefinitionRaw (..),
    RootOperationTypeDefinition (..),
  )
where

import Data.HashMap.Lazy
  ( HashMap,
    union,
  )
import qualified Data.HashMap.Lazy as HM
import Data.List (find)
-- MORPHEUS

import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
import Data.Morpheus.Error.Schema (nameCollisionError)
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    KeyOf (..),
    Listable (..),
    Merge (..),
    Selectable (..),
    elems,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    renderIndent,
    renderObject,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( DataFingerprint (..),
    Description,
    FALSE,
    FieldName,
    FieldName (..),
    GQLError (..),
    Msg (..),
    Nullable (..),
    OperationType,
    Position,
    TRUE,
    Token,
    TypeKind (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    isNotSystemTypeName,
    msg,
    sysFields,
    toFieldName,
    toOperationType,
  )
import Data.Morpheus.Types.Internal.AST.DirectiveLocation (DirectiveLocation)
import Data.Morpheus.Types.Internal.AST.OrderedMap
  ( OrderedMap,
    unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( CONST,
    Stage,
    VALID,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( ScalarValue (..),
    ValidValue,
    Value (..),
  )
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Failure (..),
    LibUpdater,
    resolveUpdates,
  )
import Data.Semigroup (Semigroup (..))
import Data.Text (intercalate)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))

type DataEnum = [DataEnumValue]

mkUnionMember :: TypeName -> UnionMember cat
mkUnionMember name = UnionMember name True

data UnionMember (cat :: TypeCategory) = UnionMember
  { memberName :: TypeName,
    visibility :: Bool
  }
  deriving (Show, Lift, Eq)

type DataUnion = [UnionMember OUT]

type DataInputUnion = [UnionMember IN]

instance RenderGQL (UnionMember cat) where
  render = render . memberName

-- scalar
------------------------------------------------------------------
newtype ScalarDefinition = ScalarDefinition
  {validateValue :: ValidValue -> Either Token ValidValue}

instance Show ScalarDefinition where
  show _ = "ScalarDefinition"

instance Lift ScalarDefinition where
  lift _ = [|ScalarDefinition pure|]

data Argument (valid :: Stage) = Argument
  { argumentName :: FieldName,
    argumentValue :: Value valid,
    argumentPosition :: Position
  }
  deriving (Show, Eq, Lift)

instance KeyOf (Argument stage) where
  keyOf = argumentName

instance NameCollision (Argument s) where
  nameCollision _ Argument {argumentName, argumentPosition} =
    GQLError
      { message = "There can Be only One Argument Named " <> msg argumentName,
        locations = [argumentPosition]
      }

type Arguments s = OrderedMap FieldName (Argument s)

-- directive
------------------------------------------------------------------
data Directive (s :: Stage) = Directive
  { directiveName :: FieldName,
    directivePosition :: Position,
    directiveArgs :: Arguments s
  }
  deriving (Show, Lift, Eq)

instance KeyOf (Directive s) where
  keyOf = directiveName

type Directives s = [Directive s]

data DirectiveDefinition = DirectiveDefinition
  { directiveDefinitionName :: FieldName,
    directiveDefinitionDescription :: Maybe Description,
    directiveDefinitionLocations :: [DirectiveLocation],
    directiveDefinitionArgs :: ArgumentsDefinition
  }
  deriving (Show, Lift)

type DirectiveDefinitions = [DirectiveDefinition]

instance KeyOf DirectiveDefinition where
  keyOf = directiveDefinitionName

instance Selectable DirectiveDefinition ArgumentDefinition where
  selectOr fb f key DirectiveDefinition {directiveDefinitionArgs} =
    selectOr fb f key directiveDefinitionArgs

lookupDeprecated :: [Directive VALID] -> Maybe (Directive VALID)
lookupDeprecated = find isDeprecation
  where
    isDeprecation Directive {directiveName = "deprecated"} = True
    isDeprecation _ = False

lookupDeprecatedReason :: Directive VALID -> Maybe Description
lookupDeprecatedReason Directive {directiveArgs} =
  selectOr Nothing (Just . maybeString) "reason" directiveArgs
  where
    maybeString :: Argument VALID -> Description
    maybeString Argument {argumentValue = (Scalar (String x))} = x
    maybeString _ = "can't read deprecated Reason Value"

-- ENUM VALUE
data DataEnumValue = DataEnumValue
  { enumName :: TypeName,
    enumDescription :: Maybe Description,
    enumDirectives :: [Directive VALID]
  }
  deriving (Show, Lift)

instance RenderGQL DataEnumValue where
  render DataEnumValue {enumName} = render enumName

-- 3.2 Schema : https://graphql.github.io/graphql-spec/June2018/#sec-Schema
---------------------------------------------------------------------------
-- SchemaDefinition :
--    schema Directives[Const](opt) { RootOperationTypeDefinition(list)}
--
-- RootOperationTypeDefinition :
--    OperationType: NamedType

data Schema = Schema
  { types :: TypeLib,
    query :: TypeDefinition 'Out,
    mutation :: Maybe (TypeDefinition 'Out),
    subscription :: Maybe (TypeDefinition 'Out)
  }
  deriving (Show)

data SchemaDefinitionRaw = SchemaDefinitionRaw
  { schemaDirectives :: Directives VALID,
    unSchemaDefinition :: OrderedMap OperationType RootOperationTypeDefinition
  }
  deriving (Show)

data RootOperationTypeDefinition = RootOperationTypeDefinition
  { rootOperationType :: OperationType,
    rootOperationTypeDefinitionName :: TypeName
  }
  deriving (Show, Eq)

instance NameCollision RootOperationTypeDefinition where
  nameCollision name _ =
    GQLError
      { message = "There can Be only One TypeDefinition for " <> msg name,
        locations = []
      }

instance KeyOf RootOperationTypeDefinition where
  type KEY RootOperationTypeDefinition = OperationType
  keyOf = rootOperationType

type TypeLib = HashMap TypeName (TypeDefinition ANY)

instance Selectable Schema (TypeDefinition ANY) where
  selectOr fb f name lib = maybe fb f (lookupDataType name lib)

instance Listable (TypeDefinition ANY) Schema where
  elems = HM.elems . typeRegister
  fromElems types = case popByKey "Query" types of
    (Nothing, _) -> failure (globalErrorMessage "INTERNAL: Query Not Defined")
    (Just query, lib1) -> do
      let (mutation, lib2) = popByKey "Mutation" lib1
      let (subscription, lib3) = popByKey "Subscription" lib2
      pure $ (foldr defineType (initTypeLib query) lib3) {mutation, subscription}

initTypeLib :: TypeDefinition 'Out -> Schema
initTypeLib query =
  Schema
    { types = empty,
      query = query,
      mutation = Nothing,
      subscription = Nothing
    }

typeRegister :: Schema -> TypeLib
typeRegister Schema {types, query, mutation, subscription} =
  types
    `union` HM.fromList
      (concatMap fromOperation [Just query, mutation, subscription])

lookupDataType :: TypeName -> Schema -> Maybe (TypeDefinition ANY)
lookupDataType name = HM.lookup name . typeRegister

isTypeDefined :: TypeName -> Schema -> Maybe DataFingerprint
isTypeDefined name lib = typeFingerprint <$> lookupDataType name lib

-- 3.4 Types : https://graphql.github.io/graphql-spec/June2018/#sec-Types
-------------------------------------------------------------------------
-- TypeDefinition :
--   ScalarTypeDefinition
--   ObjectTypeDefinition
--   InterfaceTypeDefinition
--   UnionTypeDefinition
--   EnumTypeDefinition
--   InputObjectTypeDefinition

data TypeDefinition (a :: TypeCategory) = TypeDefinition
  { typeName :: TypeName,
    typeFingerprint :: DataFingerprint,
    typeDescription :: Maybe Description,
    typeDirectives :: Directives VALID,
    typeContent :: TypeContent TRUE a
  }
  deriving (Show, Lift)

instance KeyOf (TypeDefinition a) where
  type KEY (TypeDefinition a) = TypeName
  keyOf = typeName

data TypeCategory = In | Out | Any

type IN = 'In

type OUT = 'Out

type ANY = 'Any

class ToAny a where
  toAny :: a (k :: TypeCategory) -> a ANY

instance ToAny TypeDefinition where
  toAny TypeDefinition {typeContent, ..} = TypeDefinition {typeContent = toAny typeContent, ..}

instance ToAny (TypeContent TRUE) where
  toAny DataScalar {..} = DataScalar {..}
  toAny DataEnum {..} = DataEnum {..}
  toAny DataInputObject {..} = DataInputObject {..}
  toAny DataInputUnion {..} = DataInputUnion {..}
  toAny DataObject {..} = DataObject {..}
  toAny DataUnion {..} = DataUnion {..}
  toAny DataInterface {..} = DataInterface {..}

instance ToAny FieldDefinition where
  toAny FieldDefinition {fieldContent, ..} = FieldDefinition {fieldContent = toAny <$> fieldContent, ..}

instance ToAny (FieldContent TRUE) where
  toAny (FieldArgs x) = FieldArgs x
  toAny (DefaultInputValue x) = DefaultInputValue x

class FromAny a (k :: TypeCategory) where
  fromAny :: a ANY -> Maybe (a k)

instance (FromAny (TypeContent TRUE) a) => FromAny TypeDefinition a where
  fromAny TypeDefinition {typeContent, ..} = bla <$> fromAny typeContent
    where
      bla x = TypeDefinition {typeContent = x, ..}

instance FromAny (TypeContent TRUE) IN where
  fromAny DataScalar {..} = Just DataScalar {..}
  fromAny DataEnum {..} = Just DataEnum {..}
  fromAny DataInputObject {..} = Just DataInputObject {..}
  fromAny DataInputUnion {..} = Just DataInputUnion {..}
  fromAny _ = Nothing

instance FromAny (TypeContent TRUE) OUT where
  fromAny DataScalar {..} = Just DataScalar {..}
  fromAny DataEnum {..} = Just DataEnum {..}
  fromAny DataObject {..} = Just DataObject {..}
  fromAny DataUnion {..} = Just DataUnion {..}
  fromAny DataInterface {..} = Just DataInterface {..}
  fromAny _ = Nothing

type family IsSelected (c :: TypeCategory) (a :: TypeCategory) :: Bool

type instance IsSelected ANY a = TRUE

type instance IsSelected OUT OUT = TRUE

type instance IsSelected IN IN = TRUE

type instance IsSelected IN OUT = FALSE

type instance IsSelected OUT IN = FALSE

type instance IsSelected a ANY = TRUE

data TypeContent (b :: Bool) (a :: TypeCategory) where
  DataScalar ::
    { dataScalar :: ScalarDefinition
    } ->
    TypeContent TRUE a
  DataEnum ::
    { enumMembers :: DataEnum
    } ->
    TypeContent TRUE a
  DataInputObject ::
    { inputObjectFields :: FieldsDefinition IN
    } ->
    TypeContent (IsSelected a IN) a
  DataInputUnion ::
    { inputUnionMembers :: DataInputUnion
    } ->
    TypeContent (IsSelected a IN) a
  DataObject ::
    { objectImplements :: [TypeName],
      objectFields :: FieldsDefinition OUT
    } ->
    TypeContent (IsSelected a OUT) a
  DataUnion ::
    { unionMembers :: DataUnion
    } ->
    TypeContent (IsSelected a OUT) a
  DataInterface ::
    { interfaceFields :: FieldsDefinition OUT
    } ->
    TypeContent (IsSelected a OUT) a

deriving instance Show (TypeContent a b)

deriving instance Lift (TypeContent a b)

createType :: TypeName -> TypeContent TRUE a -> TypeDefinition a
createType typeName typeContent =
  TypeDefinition
    { typeName,
      typeDescription = Nothing,
      typeFingerprint = DataFingerprint typeName [],
      typeDirectives = [],
      typeContent
    }

createScalarType :: TypeName -> TypeDefinition a
createScalarType typeName = createType typeName $ DataScalar (ScalarDefinition pure)

createEnumType :: TypeName -> [TypeName] -> TypeDefinition a
createEnumType typeName typeData = createType typeName (DataEnum enumValues)
  where
    enumValues = map createEnumValue typeData

createEnumValue :: TypeName -> DataEnumValue
createEnumValue enumName =
  DataEnumValue
    { enumName,
      enumDescription = Nothing,
      enumDirectives = []
    }

createUnionType :: TypeName -> [TypeName] -> TypeDefinition OUT
createUnionType typeName typeData = createType typeName (DataUnion $ map mkUnionMember typeData)

isEntNode :: TypeContent TRUE a -> Bool
isEntNode DataScalar {} = True
isEntNode DataEnum {} = True
isEntNode _ = False

kindOf :: TypeDefinition a -> TypeKind
kindOf TypeDefinition {typeName, typeContent} = __kind typeContent
  where
    __kind DataScalar {} = KindScalar
    __kind DataEnum {} = KindEnum
    __kind DataInputObject {} = KindInputObject
    __kind DataObject {} = KindObject (toOperationType typeName)
    __kind DataUnion {} = KindUnion
    __kind DataInputUnion {} = KindInputUnion
    __kind DataInterface {} = KindInterface

fromOperation :: Maybe (TypeDefinition OUT) -> [(TypeName, TypeDefinition ANY)]
fromOperation (Just datatype) = [(typeName datatype, toAny datatype)]
fromOperation Nothing = []

defineType :: TypeDefinition cat -> Schema -> Schema
defineType dt@TypeDefinition {typeName, typeContent = DataInputUnion enumKeys, typeFingerprint} lib =
  lib {types = HM.insert name unionTags (HM.insert typeName (toAny dt) (types lib))}
  where
    name = typeName <> "Tags"
    unionTags =
      TypeDefinition
        { typeName = name,
          typeFingerprint,
          typeDescription = Nothing,
          typeDirectives = [],
          typeContent = DataEnum $ map (createEnumValue . memberName) enumKeys
        }
defineType datatype lib =
  lib {types = HM.insert (typeName datatype) (toAny datatype) (types lib)}

insertType ::
  TypeDefinition ANY ->
  TypeUpdater
insertType datatype@TypeDefinition {typeName} lib = case isTypeDefined typeName lib of
  Nothing -> resolveUpdates (defineType datatype lib) []
  Just fingerprint
    | fingerprint == typeFingerprint datatype -> return lib
    -- throw error if 2 different types has same name
    | otherwise -> failure $ nameCollisionError typeName

updateSchema ::
  TypeName ->
  DataFingerprint ->
  [TypeUpdater] ->
  (a -> TypeDefinition cat) ->
  a ->
  TypeUpdater
updateSchema name fingerprint stack f x lib =
  case isTypeDefined name lib of
    Nothing ->
      resolveUpdates
        (defineType (f x) lib)
        stack
    Just fingerprint' | fingerprint' == fingerprint -> return lib
    -- throw error if 2 different types has same name
    Just _ -> failure $ nameCollisionError name

lookupWith :: Eq k => (a -> k) -> k -> [a] -> Maybe a
lookupWith f key = find ((== key) . f)

-- lookups and removes TypeDefinition from hashmap
popByKey :: TypeName -> [TypeDefinition ANY] -> (Maybe (TypeDefinition OUT), [TypeDefinition ANY])
popByKey name types = case lookupWith typeName name types of
  Just dt@TypeDefinition {typeContent = DataObject {}} ->
    (fromAny dt, filter ((/= name) . typeName) types)
  _ -> (Nothing, types)

newtype Fields def = Fields
  {unFields :: OrderedMap FieldName def}
  deriving
    ( Show,
      Lift,
      Functor,
      Foldable,
      Traversable
    )

deriving instance (KEY def ~ FieldName, KeyOf def) => Collection def (Fields def)

instance Merge (FieldsDefinition cat) where
  merge path (Fields x) (Fields y) = Fields <$> merge path x y

instance Selectable (Fields (FieldDefinition cat)) (FieldDefinition cat) where
  selectOr fb f name (Fields lib) = selectOr fb f name lib

unsafeFromFields :: [FieldDefinition cat] -> FieldsDefinition cat
unsafeFromFields = Fields . unsafeFromValues

fieldsToArguments :: FieldsDefinition IN -> ArgumentsDefinition
fieldsToArguments = ArgumentsDefinition Nothing . unFields

instance (KEY def ~ FieldName, KeyOf def, NameCollision def) => Listable def (Fields def) where
  fromElems = fmap Fields . fromElems
  elems = elems . unFields

-- 3.6 Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Objects
------------------------------------------------------------------------------
--  ObjectTypeDefinition:
--    Description(opt) type Name ImplementsInterfaces(opt) Directives(Const)(opt) FieldsDefinition(opt)
--
--  ImplementsInterfaces
--    implements &(opt) NamedType
--    ImplementsInterfaces & NamedType
--
--  FieldsDefinition
--    { FieldDefinition(list) }
--
type FieldsDefinition cat = Fields (FieldDefinition cat)

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
-- https://spec.graphql.org/June2018/#InputValueDefinition
-- InputValueDefinition
--   Description(opt) Name: Type DefaultValue(opt) Directives[Const](opt)

data FieldDefinition (cat :: TypeCategory) = FieldDefinition
  { fieldName :: FieldName,
    fieldDescription :: Maybe Description,
    fieldType :: TypeRef,
    fieldContent :: Maybe (FieldContent TRUE cat),
    fieldDirectives :: [Directive VALID]
  }
  deriving (Show, Lift)

data FieldContent (bool :: Bool) (cat :: TypeCategory) where
  DefaultInputValue ::
    { defaultInputValue :: Value CONST
    } ->
    FieldContent (IsSelected cat IN) cat
  FieldArgs ::
    { fieldArgsDef :: ArgumentsDefinition
    } ->
    FieldContent (IsSelected cat OUT) cat

fieldContentArgs :: FieldContent b cat -> ArgumentsDefinition
fieldContentArgs (FieldArgs args) = args
fieldContentArgs _ = empty

deriving instance Show (FieldContent bool cat)

deriving instance Lift (FieldContent bool cat)

instance KeyOf (FieldDefinition cat) where
  keyOf = fieldName

instance Selectable (FieldDefinition OUT) ArgumentDefinition where
  selectOr fb f key FieldDefinition {fieldContent = Just (FieldArgs args)} = selectOr fb f key args
  selectOr fb _ _ _ = fb

instance NameCollision (FieldDefinition cat) where
  nameCollision name _ =
    GQLError
      { message = "There can Be only One field Named " <> msg name,
        locations = []
      }

instance RenderGQL (FieldDefinition cat) where
  render FieldDefinition {fieldName = FieldName name, fieldType, fieldContent = Just (FieldArgs args)} =
    name <> render args <> ": " <> render fieldType
  render FieldDefinition {fieldName = FieldName name, fieldType} =
    name <> ": " <> render fieldType

instance RenderGQL (FieldsDefinition cat) where
  render = renderObject render . ignoreHidden . elems

instance Nullable (FieldDefinition cat) where
  isNullable = isNullable . fieldType
  toNullable field = field {fieldType = toNullable (fieldType field)}

fieldVisibility :: FieldDefinition cat -> Bool
fieldVisibility FieldDefinition {fieldName} = fieldName `notElem` sysFields

createField ::
  Maybe (FieldContent TRUE cat) ->
  FieldName ->
  [TypeWrapper] ->
  TypeName ->
  FieldDefinition cat
createField fieldContent fieldName typeWrappers typeConName =
  FieldDefinition
    { fieldName,
      fieldContent,
      fieldDescription = Nothing,
      fieldType = TypeRef {typeConName, typeWrappers, typeArgs = Nothing},
      fieldDirectives = []
    }

mkInputValue :: FieldName -> [TypeWrapper] -> TypeName -> FieldDefinition cat
mkInputValue = createField Nothing

mkObjectField ::
  ArgumentsDefinition ->
  FieldName ->
  [TypeWrapper] ->
  TypeName ->
  FieldDefinition OUT
mkObjectField args = createField (Just $ FieldArgs args)

toListField :: FieldDefinition cat -> FieldDefinition cat
toListField dataField = dataField {fieldType = listW (fieldType dataField)}
  where
    listW alias@TypeRef {typeWrappers} =
      alias {typeWrappers = TypeList : typeWrappers}

-- 3.10 Input Objects: https://spec.graphql.org/June2018/#sec-Input-Objects
---------------------------------------------------------------------------
-- InputObjectTypeDefinition
-- Description(opt) input Name Directives(const,opt) InputFieldsDefinition(opt)
--
--- InputFieldsDefinition
-- { InputValueDefinition(list) }

type InputFieldsDefinition = Fields InputValueDefinition

type InputValueDefinition = FieldDefinition IN

-- 3.6.1 Field Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
-----------------------------------------------------------------------------------------------
-- ArgumentsDefinition:
--   (InputValueDefinition(list))

data ArgumentsDefinition = ArgumentsDefinition
  { argumentsTypename :: Maybe TypeName,
    arguments :: OrderedMap FieldName ArgumentDefinition
  }
  deriving (Show, Lift)

instance RenderGQL ArgumentsDefinition where
  render ArgumentsDefinition {arguments}
    | null arguments =
      ""
    | otherwise = "(" <> intercalate ", " (map render $ elems arguments) <> ")"

type ArgumentDefinition = FieldDefinition IN

instance Selectable ArgumentsDefinition ArgumentDefinition where
  selectOr fb f key (ArgumentsDefinition _ args) = selectOr fb f key args

instance Collection ArgumentDefinition ArgumentsDefinition where
  empty = ArgumentsDefinition Nothing empty
  singleton = ArgumentsDefinition Nothing . singleton

instance Listable ArgumentDefinition ArgumentsDefinition where
  elems (ArgumentsDefinition _ args) = elems args
  fromElems args = ArgumentsDefinition Nothing <$> fromElems args

-- https://spec.graphql.org/June2018/#InputValueDefinition
-- InputValueDefinition
--   Description(opt) Name: TypeDefaultValue(opt) Directives[Const](opt)
-- TODO: implement inputValue

__inputname :: FieldName
__inputname = "inputname"

mkInputUnionFields :: TypeName -> [UnionMember IN] -> FieldsDefinition IN
mkInputUnionFields name members = unsafeFromFields $ fieldTag : map mkUnionField members
  where
    fieldTag =
      FieldDefinition
        { fieldName = __inputname,
          fieldDescription = Nothing,
          fieldContent = Nothing,
          fieldType = createAlias (name <> "Tags"),
          fieldDirectives = []
        }

mkUnionField :: UnionMember IN -> FieldDefinition IN
mkUnionField UnionMember {memberName} =
  FieldDefinition
    { fieldName = toFieldName memberName,
      fieldDescription = Nothing,
      fieldContent = Nothing,
      fieldType =
        TypeRef
          { typeConName = memberName,
            typeWrappers = [TypeMaybe],
            typeArgs = Nothing
          },
      fieldDirectives = []
    }

--
-- OTHER
--------------------------------------------------------------------------------------------------

createAlias :: TypeName -> TypeRef
createAlias typeConName =
  TypeRef {typeConName, typeWrappers = [], typeArgs = Nothing}

type TypeUpdater = LibUpdater Schema

instance RenderGQL Schema where
  render schema = intercalate "\n\n" $ map render visibleTypes
    where
      visibleTypes = filter (isNotSystemTypeName . typeName) (elems schema)

instance RenderGQL (TypeDefinition a) where
  render TypeDefinition {typeName, typeContent} = __render typeContent
    where
      __render DataInterface {interfaceFields} = "interface " <> render typeName <> render interfaceFields
      __render DataScalar {} = "scalar " <> render typeName
      __render (DataEnum tags) = "enum " <> render typeName <> renderObject render tags
      __render (DataUnion members) =
        "union "
          <> render typeName
          <> " =\n    "
          <> intercalate ("\n" <> renderIndent <> "| ") (map render members)
      __render (DataInputObject fields) = "input " <> render typeName <> render fields
      __render (DataInputUnion members) = "input " <> render typeName <> render fields
        where
          fields = mkInputUnionFields typeName members
      __render DataObject {objectFields} = "type " <> render typeName <> render objectFields

ignoreHidden :: [FieldDefinition cat] -> [FieldDefinition cat]
ignoreHidden = filter fieldVisibility
