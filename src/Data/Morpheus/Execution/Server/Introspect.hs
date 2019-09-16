{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Execution.Server.Introspect
  ( TypeUpdater
  , Introspect(..)
  , ObjectFields(..)
  , GRep(..)
  , Context(..)
  , resolveTypes
  , updateLib
  , buildType
  ) where

import           Control.Monad                                   (foldM)
import           Data.Function                                   ((&))
import           Data.Map                                        (Map)
import           Data.Proxy                                      (Proxy (..))
import           Data.Semigroup                                  ((<>))
import           Data.Set                                        (Set)
import           Data.Text                                       (Text, pack)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Schema                      (nameCollisionError)
import           Data.Morpheus.Execution.Server.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Kind                              (ENUM, GQL_KIND, INPUT_OBJECT, INPUT_UNION, OBJECT,
                                                                  SCALAR, UNION)
import           Data.Morpheus.Types.Custom                      (MapKind, Pair)
import           Data.Morpheus.Types.GQLScalar                   (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                     (GQLType (..))
import           Data.Morpheus.Types.Internal.Data               (DataArguments, DataField (..), DataFullType (..),
                                                                  DataLeaf (..), DataType (..), DataTypeLib,
                                                                  DataTypeWrapper (..), defineType, isTypeDefined)
import           Data.Morpheus.Types.Internal.Validation         (SchemaValidation)

--type ObjectConstraint a =
-- | context , like Proxy with multiple parameters
-- * 'a': actual gql type
-- * 'kind': object, scalar, enum ...
data Context a (kind :: GQL_KIND) =
  Context

-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class Introspect a where
  field :: proxy a -> Text -> DataField
  introspect :: proxy a -> TypeUpdater
  -----------------------------------------------
  default field :: (IntrospectKind (KIND a) a) =>
    proxy a -> Text -> DataField
  field _ = __field (Context :: Context a (KIND a))

instance {-# OVERLAPPABLE #-} (IntrospectKind (KIND a) a) => Introspect a where
  introspect _ = __introspect (Context :: Context a (KIND a))

-- Maybe
instance Introspect a => Introspect (Maybe a) where
  field _ = maybeField . field (Proxy @a)
    where
      maybeField dataField@DataField {fieldTypeWrappers = NonNullType:xs} = dataField {fieldTypeWrappers = xs}
      maybeField dataField                                                = dataField
  introspect _ = introspect (Proxy @a)

-- List
instance Introspect a => Introspect [a] where
  field _ = listField . field (Proxy @a)
    where
      listField :: DataField -> DataField
      listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}
  introspect _ = introspect (Proxy @a)

-- Tuple
instance Introspect (Pair k v) => Introspect (k, v) where
  field _ = field (Proxy @(Pair k v))
  introspect _ = introspect (Proxy @(Pair k v))

-- Set
instance Introspect [a] => Introspect (Set a) where
  field _ = field (Proxy @[a])
  introspect _ = introspect (Proxy @[a])

-- Map
instance Introspect (MapKind k v Maybe) => Introspect (Map k v) where
  field _ = field (Proxy @(MapKind k v Maybe))
  introspect _ = introspect (Proxy @(MapKind k v Maybe))

-- Resolver : a -> Resolver b
instance (ObjectFields a, Introspect b) => Introspect (a -> m b) where
  field _ name = (field (Proxy @b) name) {fieldArgs = fst $ objectFields (Proxy @a)}
  introspect _ typeLib = resolveTypes typeLib (introspect (Proxy @b) : argTypes)
    where
      argTypes :: [TypeUpdater]
      argTypes = snd $ objectFields (Proxy @a)

-- | Introspect With specific Kind: 'kind': object, scalar, enum ...
class IntrospectKind (kind :: GQL_KIND) a where
  __field :: Context a kind -> Text -> DataField
  default __field :: GQLType a =>
    Context a kind -> Text -> DataField
  __field _ = buildField (Proxy @a) []
    --   generates data field representation of object field
    --   according to parameter 'args' it could be
    --   * input object field: if args is '()'
    --   * object: if args is 'DataArguments'
  __introspect :: Context a kind -> TypeUpdater -- Generates internal GraphQL Schema

-- SCALAR
instance (GQLType a, GQLScalar a) => IntrospectKind SCALAR a where
  __introspect _ = updateLib scalarType [] (Proxy @a)
    where
      scalarType = Leaf . CustomScalar . buildType (scalarValidator (Proxy @a))

-- ENUM
instance (GQL_TYPE a, EnumRep (Rep a)) => IntrospectKind ENUM a where
  __introspect _ = updateLib enumType [] (Proxy @a)
    where
      enumType = Leaf . LeafEnum . buildType (enumTags (Proxy @(Rep a)))

-- INPUT_OBJECT
instance (GQL_TYPE a, ObjectFields a) => IntrospectKind INPUT_OBJECT a where
  __introspect _ = updateLib (InputObject . buildType fields) types (Proxy @a)
    where
      (fields, types) = objectFields (Proxy @a)

-- OBJECTS
instance (GQL_TYPE a, ObjectFields a) => IntrospectKind OBJECT a where
  __introspect _ = updateLib (OutputObject . buildType (__typename : fields)) types (Proxy @a)
    where
      __typename =
        ( "__typename"
        , DataField
            {fieldName = "__typename", fieldArgs = [], fieldTypeWrappers = [], fieldType = "String", fieldHidden = True})
      (fields, types) = objectFields (Proxy @a)

-- UNION
instance (GQL_TYPE a, GRep UNION a) => IntrospectKind UNION a where
  __introspect _ = updateLib (Union . buildType fields) stack (Proxy @a)
    where
      (fields, stack) = unzip $ possibleTypes (Context :: Context a UNION)

-- INPUT_UNION
instance (GQL_TYPE a, GRep UNION a) => IntrospectKind INPUT_UNION a where
  __introspect _ = updateLib (InputUnion . buildType (fieldTag : fields)) (tagsEnumType : stack) (Proxy @a)
    where
      (fields, stack) = unzip $ possibleTypes (Context :: Context a UNION)
      -- for every input Union 'User' adds enum type of possible TypeNames 'UserTags'
      tagsEnumType :: TypeUpdater
      tagsEnumType x = pure $ defineType (enumTypeName, Leaf $ LeafEnum tagsEnum) x
        where
          tagsEnum =
            DataType
              { typeName = enumTypeName
              -- has same fingerprint as object because it depends on it
              , typeFingerprint = __typeFingerprint (Proxy @a)
              , typeVisibility = __typeVisibility (Proxy @a)
              , typeDescription = ""
              , typeData = map fieldName fields
              }
      enumTypeName = __typeName (Proxy @a) <> "Tags"
      fieldTag =
        DataField
          { fieldName = "tag"
          , fieldArgs = []
          , fieldTypeWrappers = [NonNullType]
          , fieldType = enumTypeName
          , fieldHidden = False
          }

-- Types
type TypeUpdater = DataTypeLib -> SchemaValidation DataTypeLib

type GQL_TYPE a = (Generic a, GQLType a)

-- Object Fields
class ObjectFields a where
  objectFields :: proxy a -> ([(Text, DataField)], [TypeUpdater])

instance {-# OVERLAPPABLE #-} GRep OBJECT a => ObjectFields a where
  objectFields _ = unzip $ objectFieldTypes (Context :: Context a OBJECT)

--  GENERIC UNION
class GRep (kind :: GQL_KIND) f where
  possibleTypes :: Context f kind -> [(DataField, TypeUpdater)]
  objectFieldTypes :: Context f kind -> [((Text, DataField), TypeUpdater)]

instance GRep kind (Rep a) => GRep kind a where
  possibleTypes _ = possibleTypes (Context :: Context (Rep a) kind)
  objectFieldTypes _ = objectFieldTypes (Context :: Context (Rep a) kind)

instance GRep kind f => GRep kind (M1 D d f) where
  possibleTypes _ = possibleTypes (Context :: Context f kind)
  objectFieldTypes _ = objectFieldTypes (Context :: Context f kind)

instance GRep kind f => GRep kind (M1 C c f) where
  possibleTypes _ = possibleTypes (Context :: Context f kind)
  objectFieldTypes _ = objectFieldTypes (Context :: Context f kind)

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (GRep UNION a, GRep UNION b) => GRep UNION (a :+: b) where
  possibleTypes _ = possibleTypes (Context :: Context a UNION) ++ possibleTypes (Context :: Context b UNION)

instance (GQL_TYPE a, Introspect a) => GRep UNION (M1 S s (Rec0 a)) where
  possibleTypes _ = [(buildField (Proxy @a) [] "", introspect (Proxy @a))]

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (GRep OBJECT a, GRep OBJECT b) => GRep OBJECT (a :*: b) where
  objectFieldTypes _ = objectFieldTypes (Context :: Context a OBJECT) ++ objectFieldTypes (Context :: Context b OBJECT)

instance (Selector s, Introspect a) => GRep OBJECT (M1 S s (Rec0 a)) where
  objectFieldTypes _ = [((name, field (Proxy @a) name), introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: M1 S s (Rec0 ()) ())

instance GRep OBJECT U1 where
  objectFieldTypes _ = []
  possibleTypes _ = []

-- Helper Functions
resolveTypes :: DataTypeLib -> [TypeUpdater] -> SchemaValidation DataTypeLib
resolveTypes = foldM (&)

buildField :: GQLType a => Proxy a -> DataArguments -> Text -> DataField
buildField proxy fieldArgs fieldName =
  DataField {fieldName, fieldArgs, fieldTypeWrappers = [NonNullType], fieldType = __typeName proxy, fieldHidden = False}

buildType :: GQLType a => t -> Proxy a -> DataType t
buildType typeData proxy =
  DataType
    { typeName = __typeName proxy
    , typeFingerprint = __typeFingerprint proxy
    , typeDescription = description proxy
    , typeVisibility = __typeVisibility proxy
    , typeData
    }

updateLib :: GQLType a => (Proxy a -> DataFullType) -> [TypeUpdater] -> Proxy a -> TypeUpdater
updateLib typeBuilder stack proxy lib' =
  case isTypeDefined (__typeName proxy) lib' of
    Nothing -> resolveTypes (defineType (__typeName proxy, typeBuilder proxy) lib') stack
    Just fingerprint'
      | fingerprint' == __typeFingerprint proxy -> return lib'
    -- throw error if 2 different types has same name
    Just _ -> Left $ nameCollisionError (__typeName proxy)
