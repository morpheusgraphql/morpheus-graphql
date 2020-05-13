{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
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

module Data.Morpheus.Types.Internal.AST.Data
  ( Arguments,
    ScalarDefinition (..),
    DataEnum,
    FieldsDefinition (..),
    ArgumentDefinition,
    DataUnion,
    ArgumentsDefinition (..),
    FieldDefinition (..),
    InputFieldsDefinition (..),
    TypeContent (..),
    TypeDefinition (..),
    Schema (..),
    DataEnumValue (..),
    TypeLib,
    Meta (..),
    Directive (..),
    TypeUpdater,
    TypeD (..),
    ConsD (..),
    GQLTypeD (..),
    TypeCategory,
    DataInputUnion,
    Argument (..),
    createField,
    createArgument,
    createEnumType,
    createScalarType,
    createType,
    createUnionType,
    createAlias,
    createInputUnionFields,
    createEnumValue,
    defineType,
    isTypeDefined,
    initTypeLib,
    isFieldNullable,
    insertType,
    fieldVisibility,
    kindOf,
    toNullableField,
    toListField,
    toHSFieldDefinition,
    isEntNode,
    lookupDeprecated,
    lookupDeprecatedReason,
    lookupWith,
    hasArguments,
    unsafeFromFields,
    unsafeFromInputFields,
    __inputname,
    updateSchema,
    OUT,
    IN,
    ANY,
    FromAny (..),
    ToAny (..),
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
import Data.Morpheus.Types.Internal.AST.Base
  ( DataFingerprint (..),
    DataTypeKind (..),
    Description,
    GQLError (..),
    Key,
    Name,
    Position,
    Stage,
    TRUE,
    TypeRef (..),
    TypeWrapper (..),
    VALID,
    hsTypeName,
    isNullable,
    sysFields,
    toOperationType,
  )
import Data.Morpheus.Types.Internal.AST.OrderedMap
  ( OrderedMap,
    unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( ScalarValue (..),
    ValidValue,
    Value (..),
  )
import Data.Morpheus.Types.Internal.Operation
  ( Empty (..),
    KeyOf (..),
    Listable (..),
    Merge (..),
    Selectable (..),
    Singleton (..),
  )
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Failure (..),
    LibUpdater,
    resolveUpdates,
  )
import Data.Semigroup ((<>), Semigroup (..))
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))

type DataEnum = [DataEnumValue]

type DataUnion = [Key]

type DataInputUnion = [(Key, Bool)]

-- scalar
------------------------------------------------------------------
newtype ScalarDefinition = ScalarDefinition
  {validateValue :: ValidValue -> Either Key ValidValue}

instance Show ScalarDefinition where
  show _ = "ScalarDefinition"

instance Lift ScalarDefinition where
  lift _ = [|ScalarDefinition pure|]

data Argument (valid :: Stage) = Argument
  { argumentName :: Name,
    argumentValue :: Value valid,
    argumentPosition :: Position
  }
  deriving (Show, Eq, Lift)

instance KeyOf (Argument stage) where
  keyOf = argumentName

instance NameCollision (Argument s) where
  nameCollision _ Argument {argumentName, argumentPosition} =
    GQLError
      { message = "There can Be only One Argument Named \"" <> argumentName <> "\"",
        locations = [argumentPosition]
      }

type Arguments s = OrderedMap (Argument s)

-- directive
------------------------------------------------------------------
data Directive = Directive
  { directiveName :: Name,
    directiveArgs :: OrderedMap (Argument VALID)
  }
  deriving (Show, Lift)

lookupDeprecated :: Meta -> Maybe Directive
lookupDeprecated Meta {metaDirectives} = find isDeprecation metaDirectives
  where
    isDeprecation Directive {directiveName = "deprecated"} = True
    isDeprecation _ = False

lookupDeprecatedReason :: Directive -> Maybe Key
lookupDeprecatedReason Directive {directiveArgs} =
  selectOr Nothing (Just . maybeString) "reason" directiveArgs
  where
    maybeString :: Argument VALID -> Name
    maybeString Argument {argumentValue = (Scalar (String x))} = x
    maybeString _ = "can't read deprecated Reason Value"

-- META
data Meta = Meta
  { metaDescription :: Maybe Description,
    metaDirectives :: [Directive]
  }
  deriving (Show, Lift)

-- ENUM VALUE
data DataEnumValue = DataEnumValue
  { enumName :: Name,
    enumMeta :: Maybe Meta
  }
  deriving (Show, Lift)

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

type TypeLib = HashMap Key (TypeDefinition ANY)

instance Selectable Schema (TypeDefinition ANY) where
  selectOr fb f name lib = maybe fb f (lookupDataType name lib)

instance Listable Schema (TypeDefinition ANY) where
  toAssoc = HM.toList . typeRegister
  fromAssoc types = case popByKey "Query" (map snd types) of
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

lookupDataType :: Key -> Schema -> Maybe (TypeDefinition ANY)
lookupDataType name = HM.lookup name . typeRegister

isTypeDefined :: Key -> Schema -> Maybe DataFingerprint
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
  { typeName :: Key,
    typeFingerprint :: DataFingerprint,
    typeMeta :: Maybe Meta,
    typeContent :: TypeContent TRUE a
  }
  deriving (Show, Lift)

instance KeyOf (TypeDefinition a) where
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

class SelectType (c :: TypeCategory) (a :: TypeCategory) where
  type IsSelected c a :: Bool

instance SelectType ANY a where
  type IsSelected ANY a = TRUE

instance SelectType OUT OUT where
  type IsSelected OUT OUT = TRUE

instance SelectType IN IN where
  type IsSelected IN IN = TRUE

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
    { inputObjectFields :: InputFieldsDefinition
    } ->
    TypeContent (IsSelected a IN) a
  DataInputUnion ::
    { inputUnionMembers :: [(Key, Bool)]
    } ->
    TypeContent (IsSelected a IN) a
  DataObject ::
    { objectImplements :: [Name],
      objectFields :: FieldsDefinition
    } ->
    TypeContent (IsSelected a OUT) a
  DataUnion ::
    { unionMembers :: DataUnion
    } ->
    TypeContent (IsSelected a OUT) a
  DataInterface ::
    { interfaceFields :: FieldsDefinition
    } ->
    TypeContent (IsSelected a OUT) a

deriving instance Show (TypeContent a b)

deriving instance Lift (TypeContent a b)

createType :: Key -> TypeContent TRUE a -> TypeDefinition a
createType typeName typeContent =
  TypeDefinition
    { typeName,
      typeMeta = Nothing,
      typeFingerprint = DataFingerprint typeName [],
      typeContent
    }

createScalarType :: Name -> TypeDefinition a
createScalarType typeName = createType typeName $ DataScalar (ScalarDefinition pure)

createEnumType :: Name -> [Key] -> TypeDefinition a
createEnumType typeName typeData = createType typeName (DataEnum enumValues)
  where
    enumValues = map createEnumValue typeData

createEnumValue :: Name -> DataEnumValue
createEnumValue enumName = DataEnumValue {enumName, enumMeta = Nothing}

createUnionType :: Key -> [Key] -> TypeDefinition OUT
createUnionType typeName typeData = createType typeName (DataUnion typeData)

isEntNode :: TypeContent TRUE a -> Bool
isEntNode DataScalar {} = True
isEntNode DataEnum {} = True
isEntNode _ = False

kindOf :: TypeDefinition a -> DataTypeKind
kindOf TypeDefinition {typeName, typeContent} = __kind typeContent
  where
    __kind DataScalar {} = KindScalar
    __kind DataEnum {} = KindEnum
    __kind DataInputObject {} = KindInputObject
    __kind DataObject {} = KindObject (toOperationType typeName)
    __kind DataUnion {} = KindUnion
    __kind DataInputUnion {} = KindInputUnion
    __kind DataInterface {} = KindInterface

fromOperation :: Maybe (TypeDefinition OUT) -> [(Name, TypeDefinition ANY)]
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
          typeMeta = Nothing,
          typeContent = DataEnum $ map (createEnumValue . fst) enumKeys
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
  Name ->
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
popByKey :: Name -> [TypeDefinition ANY] -> (Maybe (TypeDefinition OUT), [TypeDefinition ANY])
popByKey name types = case lookupWith typeName name types of
  Just dt@TypeDefinition {typeContent = DataObject {}} ->
    (fromAny dt, filter ((/= name) . typeName) types)
  _ -> (Nothing, types)

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
newtype FieldsDefinition = FieldsDefinition
  {unFieldsDefinition :: OrderedMap FieldDefinition}
  deriving (Show, Empty, Lift)

unsafeFromFields :: [FieldDefinition] -> FieldsDefinition
unsafeFromFields = FieldsDefinition . unsafeFromValues

instance Merge FieldsDefinition where
  merge path (FieldsDefinition x) (FieldsDefinition y) = FieldsDefinition <$> merge path x y

instance Selectable FieldsDefinition FieldDefinition where
  selectOr fb f name (FieldsDefinition lib) = selectOr fb f name lib

instance Singleton FieldsDefinition FieldDefinition where
  singleton = FieldsDefinition . singleton

instance Listable FieldsDefinition FieldDefinition where
  fromAssoc ls = FieldsDefinition <$> fromAssoc ls
  toAssoc = toAssoc . unFieldsDefinition

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
data FieldDefinition = FieldDefinition
  { fieldName :: Key,
    fieldArgs :: ArgumentsDefinition,
    fieldType :: TypeRef,
    fieldMeta :: Maybe Meta
  }
  deriving (Show, Lift)

instance KeyOf FieldDefinition where
  keyOf = fieldName

instance Selectable FieldDefinition ArgumentDefinition where
  selectOr fb f key FieldDefinition {fieldArgs} = selectOr fb f key fieldArgs

instance NameCollision FieldDefinition where
  nameCollision name _ =
    GQLError
      { message = "There can Be only One field Named \"" <> name <> "\"",
        locations = []
      }

fieldVisibility :: FieldDefinition -> Bool
fieldVisibility FieldDefinition {fieldName} = fieldName `notElem` sysFields

isFieldNullable :: FieldDefinition -> Bool
isFieldNullable = isNullable . fieldType

createField :: ArgumentsDefinition -> Key -> ([TypeWrapper], Key) -> FieldDefinition
createField dataArguments fieldName (typeWrappers, typeConName) =
  FieldDefinition
    { fieldArgs = dataArguments,
      fieldName,
      fieldType = TypeRef {typeConName, typeWrappers, typeArgs = Nothing},
      fieldMeta = Nothing
    }

toHSFieldDefinition :: FieldDefinition -> FieldDefinition
toHSFieldDefinition field@FieldDefinition {fieldType = tyRef@TypeRef {typeConName}} =
  field
    { fieldType = tyRef {typeConName = hsTypeName typeConName}
    }

toNullableField :: FieldDefinition -> FieldDefinition
toNullableField dataField
  | isNullable (fieldType dataField) = dataField
  | otherwise = dataField {fieldType = nullable (fieldType dataField)}
  where
    nullable alias@TypeRef {typeWrappers} =
      alias {typeWrappers = TypeMaybe : typeWrappers}

toListField :: FieldDefinition -> FieldDefinition
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

newtype InputFieldsDefinition = InputFieldsDefinition
  {unInputFieldsDefinition :: OrderedMap FieldDefinition}
  deriving (Show, Empty, Lift)

unsafeFromInputFields :: [FieldDefinition] -> InputFieldsDefinition
unsafeFromInputFields = InputFieldsDefinition . unsafeFromValues

instance Merge InputFieldsDefinition where
  merge path (InputFieldsDefinition x) (InputFieldsDefinition y) = InputFieldsDefinition <$> merge path x y

instance Selectable InputFieldsDefinition FieldDefinition where
  selectOr fb f name (InputFieldsDefinition lib) = selectOr fb f name lib

instance Singleton InputFieldsDefinition FieldDefinition where
  singleton = InputFieldsDefinition . singleton

instance Listable InputFieldsDefinition FieldDefinition where
  fromAssoc ls = InputFieldsDefinition <$> fromAssoc ls
  toAssoc = toAssoc . unInputFieldsDefinition

-- 3.6.1 Field Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
-----------------------------------------------------------------------------------------------
-- ArgumentsDefinition:
--   (InputValueDefinition(list))

data ArgumentsDefinition
  = ArgumentsDefinition
      { argumentsTypename :: Maybe Name,
        arguments :: OrderedMap ArgumentDefinition
      }
  | NoArguments
  deriving (Show, Lift)

type ArgumentDefinition = FieldDefinition

instance Selectable ArgumentsDefinition ArgumentDefinition where
  selectOr fb _ _ NoArguments = fb
  selectOr fb f key (ArgumentsDefinition _ args) = selectOr fb f key args

instance Singleton ArgumentsDefinition ArgumentDefinition where
  singleton = ArgumentsDefinition Nothing . singleton

instance Listable ArgumentsDefinition ArgumentDefinition where
  toAssoc NoArguments = []
  toAssoc (ArgumentsDefinition _ args) = toAssoc args
  fromAssoc [] = pure NoArguments
  fromAssoc args = ArgumentsDefinition Nothing <$> fromAssoc args

createArgument :: Key -> ([TypeWrapper], Key) -> FieldDefinition
createArgument = createField NoArguments

hasArguments :: ArgumentsDefinition -> Bool
hasArguments NoArguments = False
hasArguments _ = True

-- https://spec.graphql.org/June2018/#InputValueDefinition
-- InputValueDefinition
--   Description(opt) Name: TypeDefaultValue(opt) Directives[Const](opt)
-- TODO: implement inputValue

-- data InputValueDefinition = InputValueDefinition
--   { inputValueName  :: Key
--   , inputValueType  :: TypeRef
--   , inputValueMeta  :: Maybe Meta
--   } deriving (Show,Lift)

__inputname :: Name
__inputname = "inputname"

createInputUnionFields :: Key -> [Key] -> [FieldDefinition]
createInputUnionFields name members = fieldTag : map unionField members
  where
    fieldTag =
      FieldDefinition
        { fieldName = __inputname,
          fieldArgs = NoArguments,
          fieldType = createAlias (name <> "Tags"),
          fieldMeta = Nothing
        }
    unionField memberName =
      FieldDefinition
        { fieldArgs = NoArguments,
          fieldName = memberName,
          fieldType =
            TypeRef
              { typeConName = memberName,
                typeWrappers = [TypeMaybe],
                typeArgs = Nothing
              },
          fieldMeta = Nothing
        }

--
-- OTHER
--------------------------------------------------------------------------------------------------

createAlias :: Key -> TypeRef
createAlias typeConName =
  TypeRef {typeConName, typeWrappers = [], typeArgs = Nothing}

type TypeUpdater = LibUpdater Schema

-- Template Haskell Types
-- Document
data GQLTypeD = GQLTypeD
  { typeD :: TypeD,
    typeArgD :: [TypeD],
    typeOriginal :: TypeDefinition ANY
  }
  deriving (Show)

--- Core
data TypeD = TypeD
  { tName :: Name,
    tNamespace :: [Name],
    tCons :: [ConsD],
    tKind :: DataTypeKind,
    tMeta :: Maybe Meta
  }
  deriving (Show)

data ConsD = ConsD
  { cName :: Name,
    cFields :: [FieldDefinition]
  }
  deriving (Show)
