{-# LANGUAGE ConstraintKinds       #-}
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
import           Data.Morpheus.Kind                              (ENUM, INPUT_OBJECT, INPUT_UNION, OBJECT, SCALAR,
                                                                  UNION, WRAPPER)
import           Data.Morpheus.Types.Custom                      (MapKind, Pair)
import           Data.Morpheus.Types.GQLScalar                   (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                     (GQLType (..))
import           Data.Morpheus.Types.Internal.Data               (DataArguments, DataField (..), DataFullType (..),
                                                                  DataInputField, DataLeaf (..), DataType (..),
                                                                  DataTypeLib, DataTypeWrapper (..), defineType,
                                                                  isTypeDefined)
import           Data.Morpheus.Types.Internal.Validation         (SchemaValidation)
import           Data.Morpheus.Types.Resolver                    (Resolver, SubResolver)

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

type EnumConstraint a = (GQL_TYPE a, EnumRep (Rep a))

type ObjectConstraint a = (GQL_TYPE a, ObjectRep (Rep a))

-- | context , like Proxy with multiple parameters
-- contains types of :
-- * 'a': actual gql type
-- * 'kind': object, scalar, enum ...
-- * 'args': InputType | OutputType
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
  fieldTypes :: proxy a -> [((Text, DataField), TypeUpdater)]
  default fieldTypes :: ObjectRep (Rep a) =>
    proxy a -> [((Text, DataField), TypeUpdater)]
  fieldTypes _ = objectFieldTypes (Proxy @(Rep a))

instance {-# OVERLAPPABLE #-} ObjectRep (Rep a) => ObjectFields a

class Introspect a where
  type IRep a :: *
  type IRep a = Context a (KIND a)
  field :: proxy a -> Text -> DataField
  default field :: Introspect1 a (KIND a) =>
    proxy a -> Text -> DataField
  field _ = __field (Context :: IRep a)
  ------------------------------------------
  introspect :: proxy a -> TypeUpdater
  default introspect :: Introspect1 a (KIND a) =>
    proxy a -> TypeUpdater
  introspect _ = __introspect (Context :: IRep a)

instance {-# OVERLAPPABLE #-} (Introspect1 a (KIND a)) => Introspect a

-- |   Generates internal GraphQL Schema for query validation and introspection rendering
-- * 'kind': object, scalar, enum ...
-- * 'args': type of field arguments
--    * '()' for 'input values' , they are just JSON properties and does not have any argument
--    * 'DataArguments' for field Resolvers Types, where 'DataArguments' is type of arguments
class Introspect1 a kind where
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
--
instance (GQLScalar a, GQLType a) => Introspect1 a SCALAR where
  __introspect _ = updateLib scalarType [] (Proxy @a)
    where
      scalarType :: Proxy a -> DataFullType
      scalarType = Leaf . CustomScalar . buildType validator
        where
          validator = scalarValidator (Proxy @a)

--
-- ENUM
--
instance EnumConstraint a => Introspect1 a ENUM where
  __introspect _ = updateLib enumType [] (Proxy @a)
    where
      enumType :: Proxy a -> DataFullType
      enumType = Leaf . LeafEnum . buildType tags
        where
          tags = enumTags (Proxy @(Rep a))

--
-- OBJECTS , INPUT_OBJECT
--
instance (GQL_TYPE a, ObjectFields a) => Introspect1 a INPUT_OBJECT where
  __introspect _ = updateLib (InputObject . buildType fields') stack' (Proxy @a)
    where
      (fields', stack') = unzip $ fieldTypes (Proxy @a)

instance (GQL_TYPE a, ObjectFields a) => Introspect1 a OBJECT where
  __introspect _ = updateLib (OutputObject . buildType (__typename : fields')) stack' (Proxy @a)
    where
      __typename =
        ( "__typename"
        , DataField
            {fieldName = "__typename", fieldArgs = [], fieldTypeWrappers = [], fieldType = "String", fieldHidden = True})
      (fields', stack') = unzip $ fieldTypes (Proxy @a)

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
maybeField :: DataField -> DataField
maybeField dataField@DataField {fieldTypeWrappers = NonNullType:xs} = dataField {fieldTypeWrappers = xs}
maybeField dataField                                                = dataField

instance Introspect a => Introspect1 (Maybe a) WRAPPER where
  __field _ name = maybeField $ field (Proxy @a) name
  __introspect _ = introspect (Proxy @a)

instance Introspect a => Introspect1 [a] WRAPPER where
  __field _ name = listField $ field (Proxy @a) name
    where
      listField :: DataField -> DataField
      listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}
  __introspect _ = introspect (Proxy @a)

--
-- CUSTOM Types: Tuple, Map, Set
--
instance Introspect1 (Pair k v) OBJECT => Introspect1 (k, v) WRAPPER where
  __field _ = __field (Context :: Context (Pair k v) OBJECT)
  __introspect _ = __introspect (Context :: Context (Pair k v) OBJECT)

instance Introspect1 [a] WRAPPER => Introspect1 (Set a) WRAPPER where
  __field _ = __field (Context :: Context [a] WRAPPER)
  __introspect _ = __introspect (Context :: Context [a] WRAPPER)

-- | introspection Does not care about resolving monad, some fake monad just for mocking
type MockRes = (Resolver Maybe)

instance Introspect1 (MapKind k v MockRes) OBJECT => Introspect1 (Map k v) WRAPPER where
  __field _ = __field (Context :: Context (MapKind k v MockRes) OBJECT)
  __introspect _ = __introspect (Context :: Context (MapKind k v MockRes) OBJECT)

-- |introspects Of Resolver 'a' as argument and 'b' as output type
instance (ObjectFields a, OutputConstraint b) => Introspect1 (a -> Resolver m b) WRAPPER where
  __field _ name = (field (Proxy @b) name) {fieldArgs = map fst $ fieldTypes (Proxy @a)}
  __introspect _ typeLib = resolveTypes typeLib $ map snd args ++ [introspect (Proxy @b)]
    where
      args :: [((Text, DataInputField), TypeUpdater)]
      args = fieldTypes (Proxy @a)

instance (ObjectFields a, OutputConstraint b) => Introspect1 (a -> Either String b) WRAPPER where
  __field _ name = (field (Proxy @b) name) {fieldArgs = map fst $ fieldTypes (Proxy @a)}
  __introspect _ typeLib = resolveTypes typeLib $ map snd args ++ [introspect (Proxy @b)]
    where
      args :: [((Text, DataInputField), TypeUpdater)]
      args = fieldTypes (Proxy @a)

instance (ObjectFields a, OutputConstraint b) => Introspect1 (a -> SubResolver m c v b) WRAPPER where
  __field _ name = (field (Proxy @b) name) {fieldArgs = map fst $ fieldTypes (Proxy @a)}
  __introspect _ typeLib = resolveTypes typeLib (introspect (Proxy @b) : argTypes)
    where
      argTypes :: [TypeUpdater]
      argTypes = map snd $ fieldTypes (Proxy @a)
