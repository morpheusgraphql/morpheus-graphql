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
  , ObjectRep(..)
  , Introspect(..)
  , ObjectFields(..)
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

type SelOf s = M1 S s (Rec0 ()) ()

type RecSel s a = M1 S s (Rec0 a)

type TypeUpdater = DataTypeLib -> SchemaValidation DataTypeLib

--
--  GENERIC UNION
--
class UnionRep f where
  possibleTypes :: Proxy f -> [(DataField, TypeUpdater)]

instance UnionRep f => UnionRep (M1 D x f) where
  possibleTypes _ = possibleTypes (Proxy @f)

instance UnionRep f => UnionRep (M1 C x f) where
  possibleTypes _ = possibleTypes (Proxy @f)

instance (UnionRep a, UnionRep b) => UnionRep (a :+: b) where
  possibleTypes _ = possibleTypes (Proxy @a) ++ possibleTypes (Proxy @b)

--
--  GENERIC OBJECT: INPUT and OUTPUT plus ARGUMENTS
--
resolveTypes :: DataTypeLib -> [TypeUpdater] -> SchemaValidation DataTypeLib
resolveTypes = foldM (&)

class ObjectRep rep where
  objectFieldTypes :: proxy rep -> [((Text, DataField), TypeUpdater)]

instance ObjectRep f => ObjectRep (M1 D x f) where
  objectFieldTypes _ = objectFieldTypes (Proxy @f)

instance ObjectRep f => ObjectRep (M1 C x f) where
  objectFieldTypes _ = objectFieldTypes (Proxy @f)

instance (ObjectRep a, ObjectRep b) => ObjectRep (a :*: b) where
  objectFieldTypes _ = objectFieldTypes (Proxy @a) ++ objectFieldTypes (Proxy @b)

instance ObjectRep U1 where
  objectFieldTypes _ = []

-- class Types class
type GQL_TYPE a = (Generic a, GQLType a)

type ObjectConstraint a = (GQL_TYPE a, ObjectRep (Rep a))

-- | context , like Proxy with multiple parameters
-- * 'a': actual gql type
-- * 'kind': object, scalar, enum ...
data Context a kind =
  Context

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

class ObjectFields a where
  objectFields :: proxy a -> ([(Text, DataField)], [TypeUpdater])
  default objectFields :: ObjectRep (Rep a) =>
    proxy a -> ([(Text, DataField)], [TypeUpdater])
  objectFields _ = unzip $ objectFieldTypes (Proxy @(Rep a))

instance {-# OVERLAPPABLE #-} ObjectRep (Rep a) => ObjectFields a

class Introspect a where
  field :: proxy a -> Text -> DataField
  introspect :: proxy a -> TypeUpdater

instance {-# OVERLAPPABLE #-} (Introspect1 a (KIND a)) => Introspect a where
  field _ = __field (Context :: Context a (KIND a))
  introspect _ = __introspect (Context :: Context a (KIND a))

-- |   Generates internal GraphQL Schema for query validation and introspection rendering
-- * 'kind': object, scalar, enum ...
-- * 'args': type of field arguments
--    * '()' for 'input values' , they are just JSON properties and does not have any argument
--    * 'DataArguments' for field Resolvers Types, where 'DataArguments' is type of arguments
class Introspect1 a (kind :: GQL_KIND) where
  __field :: Context a kind -> Text -> DataField
  default __field :: GQLType a =>
    Context a kind -> Text -> DataField
  __field _ = buildField (Proxy @a) []
    --   generates data field representation of object field
    --   according to parameter 'args' it could be
    --   * input object field: if args is '()'
    --   * object: if args is 'DataArguments'
  __introspect :: Context a kind -> TypeUpdater -- Generates internal GraphQL Schema

type OutputConstraint a = Introspect a

--
-- SCALAR
instance (GQLScalar a, GQLType a) => Introspect1 a SCALAR where
  __introspect _ = updateLib scalarType [] (Proxy @a)
    where
      scalarType = Leaf . CustomScalar . buildType validator
        where
          validator = scalarValidator (Proxy @a)

--
-- ENUM
instance (GQL_TYPE a, EnumRep (Rep a)) => Introspect1 a ENUM where
  __introspect _ = updateLib enumType [] (Proxy @a)
    where
      enumType = Leaf . LeafEnum . buildType tags
        where
          tags = enumTags (Proxy @(Rep a))

--
-- OBJECTS , INPUT_OBJECT
instance (GQL_TYPE a, ObjectFields a) => Introspect1 a INPUT_OBJECT where
  __introspect _ = updateLib (InputObject . buildType fields) types (Proxy @a)
    where
      (fields, types) = objectFields (Proxy @a)

instance (GQL_TYPE a, ObjectFields a) => Introspect1 a OBJECT where
  __introspect _ = updateLib (OutputObject . buildType (__typename : fields)) types (Proxy @a)
    where
      __typename =
        ( "__typename"
        , DataField
            {fieldName = "__typename", fieldArgs = [], fieldTypeWrappers = [], fieldType = "String", fieldHidden = True})
      (fields, types) = objectFields (Proxy @a)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
-- iterates on field types  and introspects them recursively
instance (Selector s, Introspect a) => ObjectRep (RecSel s a) where
  objectFieldTypes _ = [((name, field (Proxy @a) name), introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: SelOf s)

--
-- UNION
-- | recursion for union types
-- iterates on possible types for UNION and introspects them recursively
instance (OutputConstraint a, ObjectConstraint a) => UnionRep (RecSel s a) where
  possibleTypes _ = [(buildField (Proxy @a) [] "", introspect (Proxy @a))]

instance (GQL_TYPE a, UnionRep (Rep a)) => Introspect1 a UNION where
  __introspect _ = updateLib (Union . buildType fields) stack (Proxy @a)
    where
      (fields, stack) = unzip $ possibleTypes (Proxy @(Rep a))

--
-- INPUT_UNION
--
instance (GQL_TYPE a, UnionRep (Rep a)) => Introspect1 a INPUT_UNION where
  __introspect _ = updateLib (InputUnion . buildType (fieldTag : fields)) (tagsEnumType : stack) (Proxy @a)
    where
      (fields, stack) = unzip $ possibleTypes (Proxy @(Rep a))
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

--
-- WRAPPER : Maybe, LIST , Resolver
--
instance Introspect a => Introspect (Maybe a) where
  field _ = maybeField . field (Proxy @a)
    where
      maybeField dataField@DataField {fieldTypeWrappers = NonNullType:xs} = dataField {fieldTypeWrappers = xs}
      maybeField dataField                                                = dataField
  introspect _ = introspect (Proxy @a)

instance Introspect a => Introspect [a] where
  field _ = listField . field (Proxy @a)
    where
      listField :: DataField -> DataField
      listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}
  introspect _ = introspect (Proxy @a)

--
-- CUSTOM Types: Tuple, Map, Set
--
instance Introspect (Pair k v) => Introspect (k, v) where
  field _ = field (Proxy @(Pair k v))
  introspect _ = introspect (Proxy @(Pair k v))

instance Introspect [a] => Introspect (Set a) where
  field _ = field (Proxy @[a])
  introspect _ = introspect (Proxy @[a])

instance Introspect (MapKind k v Maybe) => Introspect (Map k v) where
  field _ = field (Proxy @(MapKind k v Maybe))
  introspect _ = introspect (Proxy @(MapKind k v Maybe))

instance (ObjectFields a, OutputConstraint b) => Introspect (a -> m b) where
  field _ name = (field (Proxy @b) name) {fieldArgs = fst $ objectFields (Proxy @a)}
  introspect _ typeLib = resolveTypes typeLib (introspect (Proxy @b) : argTypes)
    where
      argTypes :: [TypeUpdater]
      argTypes = snd $ objectFields (Proxy @a)
