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
  ( ScalarDefinition (..),
    DataEnum,
    DataUnion,
    TypeContent (..),
    TypeDefinition (..),
    Schema (..),
    DataEnumValue (..),
    TypeLib,
    TypeUpdater,
    TypeCategory,
    DataInputUnion,
    mkEnumContent,
    createScalarType,
    mkType,
    createUnionType,
    createAlias,
    mkInputUnionFields,
    defineType,
    initTypeLib,
    insertType,
    kindOf,
    isEntNode,
    lookupWith,
    __inputname,
    updateSchema,
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
    FieldName,
    FieldName (..),
    GQLError (..),
    Msg (..),
    OperationType,
    TRUE,
    Token,
    TypeKind (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    isNotSystemTypeName,
    msg,
    toFieldName,
    toOperationType,
  )
import Data.Morpheus.Types.Internal.AST.Fields
  ( Directive,
    Directives,
    FieldDefinition (..),
    FieldsDefinition,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.AST.OrderedMap
  ( OrderedMap,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( VALID,
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( ANY,
    FromAny (..),
    IN,
    IsSelected,
    OUT,
    ToAny (..),
    TypeCategory,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( Value (..),
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
  {validateValue :: Value VALID -> Either Token (Value VALID)}

instance Show ScalarDefinition where
  show _ = "ScalarDefinition"

instance Lift ScalarDefinition where
  lift _ = [|ScalarDefinition pure|]

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
    query :: TypeDefinition OUT,
    mutation :: Maybe (TypeDefinition OUT),
    subscription :: Maybe (TypeDefinition OUT)
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

initTypeLib :: TypeDefinition OUT -> Schema
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

mkType :: TypeName -> TypeContent TRUE a -> TypeDefinition a
mkType typeName typeContent =
  TypeDefinition
    { typeName,
      typeDescription = Nothing,
      typeFingerprint = DataFingerprint typeName [],
      typeDirectives = [],
      typeContent
    }

createScalarType :: TypeName -> TypeDefinition a
createScalarType typeName = mkType typeName $ DataScalar (ScalarDefinition pure)

mkEnumContent :: [TypeName] -> TypeContent TRUE a
mkEnumContent typeData = DataEnum (map mkEnumValue typeData)

mkEnumValue :: TypeName -> DataEnumValue
mkEnumValue enumName =
  DataEnumValue
    { enumName,
      enumDescription = Nothing,
      enumDirectives = []
    }

createUnionType :: TypeName -> [TypeName] -> TypeDefinition OUT
createUnionType typeName typeData = mkType typeName (DataUnion $ map mkUnionMember typeData)

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
          typeContent = mkEnumContent (map memberName enumKeys)
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
