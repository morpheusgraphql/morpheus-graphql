{-# LANGUAGE ConstraintKinds       #-}
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
  ( introspectOutputType
  , TypeUpdater
  , ObjectRep(..)
  , Introspect(..)
  , resolveTypes
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
                                                                  DataTypeLib, DataTypeWrapper (..), DataValidator,
                                                                  defineType, isTypeDefined)
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
  objectFieldTypes :: Proxy rep -> [((Text, DataField), TypeUpdater)]

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

scalarTypeOf :: GQLType a => DataValidator -> Proxy a -> DataFullType
scalarTypeOf validator = Leaf . CustomScalar . buildType validator

enumTypeOf :: GQLType a => [Text] -> Proxy a -> DataFullType
enumTypeOf tags' = Leaf . LeafEnum . buildType tags'

type InputOf t = Context t (KIND t)

type OutputOf t = Context t (KIND t)

introspectOutputType ::
     forall a. Introspect a (KIND a)
  => Proxy a
  -> TypeUpdater
introspectOutputType _ = introspect (Context :: OutputOf a)

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

class Intro a where
  intro :: a -> TypeUpdater

instance Introspect a (KIND a) => Intro a where
  intro _ = introspect (Context :: Context a (KIND a))

-- |   Generates internal GraphQL Schema for query validation and introspection rendering
-- * 'kind': object, scalar, enum ...
-- * 'args': type of field arguments
--    * '()' for 'input values' , they are just JSON properties and does not have any argument
--    * 'DataArguments' for field Resolvers Types, where 'DataArguments' is type of arguments
class Introspect a kind where
  __field :: Context a kind -> Text -> DataField
    --   generates data field representation of object field
    --   according to parameter 'args' it could be
    --   * input object field: if args is '()'
    --   * object: if args is 'DataArguments'
  introspect :: Context a kind -> TypeUpdater -- Generates internal GraphQL Schema

type OutputConstraint a = Introspect a (KIND a)

--
-- SCALAR
--
instance (GQLScalar a, GQLType a) => Introspect a SCALAR where
  __field _ = buildField (Proxy @a) []
  introspect _ = updateLib (scalarTypeOf (scalarValidator $ Proxy @a)) [] (Proxy @a)

--
-- ENUM
--
instance EnumConstraint a => Introspect a ENUM where
  __field _ = buildField (Proxy @a) []
  introspect _ = updateLib (enumTypeOf $ enumTags (Proxy @(Rep a))) [] (Proxy @a)

--
-- OBJECTS , INPUT_OBJECT
--
instance ObjectConstraint a => Introspect a INPUT_OBJECT where
  __field _ = buildField (Proxy @a) []
  introspect _ = updateLib (InputObject . buildType fields') stack' (Proxy @a)
    where
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

instance ObjectConstraint a => Introspect a OBJECT where
  __field _ = buildField (Proxy @a) []
  introspect _ = updateLib (OutputObject . buildType (__typename : fields')) stack' (Proxy @a)
    where
      __typename =
        ( "__typename"
        , DataField
            {fieldName = "__typename", fieldArgs = [], fieldTypeWrappers = [], fieldType = "String", fieldHidden = True})
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
-- iterates on field types  and introspects them recursively
instance (Selector s, Introspect a (KIND a)) => ObjectRep (RecSel s a) where
  objectFieldTypes _ =
    [((name, __field (Context :: Context a (KIND a)) name), introspect (Context :: Context a (KIND a)))]
    where
      name = pack $ selName (undefined :: SelOf s)

--
-- UNION
-- | recursion for union types
-- iterates on possible types for UNION and introspects them recursively
instance (OutputConstraint a, ObjectConstraint a) => UnionRep (RecSel s a) where
  possibleTypes _ = [(buildField (Proxy @a) [] "", introspect (Context :: OutputOf a))]

instance (GQL_TYPE a, UnionRep (Rep a)) => Introspect a UNION where
  __field _ = buildField (Proxy @a) []
  introspect _ = updateLib (Union . buildType fields) stack (Proxy @a)
    where
      (fields, stack) = unzip $ possibleTypes (Proxy @(Rep a))

--
-- INPUT_UNION
--
instance (GQL_TYPE a, UnionRep (Rep a)) => Introspect a INPUT_UNION where
  __field _ = buildField (Proxy @a) []
  introspect _ = updateLib (InputUnion . buildType (fieldTag : fields)) (tagsEnumType : stack) (Proxy @a)
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
maybeField field@DataField {fieldTypeWrappers = NonNullType:xs} = field {fieldTypeWrappers = xs}
maybeField field                                                = field

instance Introspect a (KIND a) => Introspect (Maybe a) WRAPPER where
  __field _ name = maybeField $ __field (Context :: Context a (KIND a)) name
  introspect _ = introspect (Context :: Context a (KIND a))

instance Introspect a (KIND a) => Introspect [a] WRAPPER where
  __field _ name = listField (__field (Context :: Context a (KIND a)) name)
    where
      listField :: DataField -> DataField
      listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}
  introspect _ = introspect (Context :: Context a (KIND a))

--
-- CUSTOM Types: Tuple, Map, Set
--
instance Introspect (Pair k v) OBJECT => Introspect (k, v) WRAPPER where
  __field _ = __field (Context :: Context (Pair k v) OBJECT)
  introspect _ = introspect (Context :: Context (Pair k v) OBJECT)

instance Introspect [a] WRAPPER => Introspect (Set a) WRAPPER where
  __field _ = __field (Context :: Context [a] WRAPPER)
  introspect _ = introspect (Context :: Context [a] WRAPPER)

-- | introspection Does not care about resolving monad, some fake monad just for mocking
type MockRes = (Resolver Maybe)

instance Introspect (MapKind k v MockRes) OBJECT => Introspect (Map k v) WRAPPER where
  __field _ = __field (Context :: Context (MapKind k v MockRes) OBJECT)
  introspect _ = introspect (Context :: Context (MapKind k v MockRes) OBJECT)

-- |introspects Of Resolver 'a' as argument and 'b' as output type
instance (ObjectRep (Rep a), OutputConstraint b) => Introspect (a -> Resolver m b) WRAPPER where
  __field _ name = (__field (Context :: OutputOf b) name) {fieldArgs = map fst $ objectFieldTypes (Proxy @(Rep a))}
  introspect _ typeLib = resolveTypes typeLib $ map snd args ++ [introspect (Context :: OutputOf b)]
    where
      args :: [((Text, DataInputField), TypeUpdater)]
      args = objectFieldTypes (Proxy @(Rep a))

instance (ObjectRep (Rep a), OutputConstraint b) => Introspect (a -> Either String b) WRAPPER where
  __field _ name = (__field (Context :: OutputOf b) name) {fieldArgs = map fst $ objectFieldTypes (Proxy @(Rep a))}
  introspect _ typeLib = resolveTypes typeLib $ map snd args ++ [introspect (Context :: OutputOf b)]
    where
      args :: [((Text, DataInputField), TypeUpdater)]
      args = objectFieldTypes (Proxy @(Rep a))

instance (ObjectRep (Rep a), OutputConstraint b) => Introspect (a -> SubResolver m c v b) WRAPPER where
  __field _ name = (__field (Context :: OutputOf b) name) {fieldArgs = map fst $ objectFieldTypes (Proxy @(Rep a))}
  introspect _ typeLib = resolveTypes typeLib $ map snd args ++ [introspect (Context :: OutputOf b)]
    where
      args :: [((Text, DataInputField), TypeUpdater)]
      args = objectFieldTypes (Proxy @(Rep a))
