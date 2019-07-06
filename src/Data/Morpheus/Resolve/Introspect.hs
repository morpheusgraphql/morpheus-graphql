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

module Data.Morpheus.Resolve.Introspect
  ( introspectOutputType
  , TypeUpdater
  , ObjectRep(..)
  , resolveTypes
  ) where

import           Control.Monad                           (foldM)
import           Data.Function                           ((&))
import           Data.Map                                (Map)
import           Data.Proxy                              (Proxy (..))
import           Data.Semigroup                          ((<>))
import           Data.Set                                (Set)
import           Data.Text                               (Text, pack)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Schema              (nameCollisionError)
import           Data.Morpheus.Kind                      (ENUM, INPUT_OBJECT, INPUT_UNION, KIND, OBJECT, SCALAR, UNION,
                                                          WRAPPER)
import           Data.Morpheus.Resolve.Generics.EnumRep  (EnumRep (..))
import           Data.Morpheus.Types.Custom              (MapKind, Pair)
import           Data.Morpheus.Types.GQLScalar           (GQLScalar (..))
import           Data.Morpheus.Types.GQLType             (GQLType (..))
import           Data.Morpheus.Types.Internal.Data       (DataArguments, DataField (..), DataFullType (..),
                                                          DataInputField, DataLeaf (..), DataType (..),
                                                          DataTypeKind (..), DataTypeLib, DataTypeWrapper (..),
                                                          DataValidator, defineType, isTypeDefined)
import           Data.Morpheus.Types.Internal.Validation (SchemaValidation)
import           Data.Morpheus.Types.Resolver            (Resolver)

type SelOf s = M1 S s (Rec0 ()) ()

type RecSel s a = M1 S s (Rec0 a)

type TypeUpdater = DataTypeLib -> SchemaValidation DataTypeLib

--
--  GENERIC UNION
--
class UnionRep f t where
  possibleTypes :: Proxy f -> Proxy t -> [(DataField (), TypeUpdater)]

instance UnionRep f t => UnionRep (M1 D x f) t where
  possibleTypes _ = possibleTypes (Proxy @f)

instance UnionRep f t => UnionRep (M1 C x f) t where
  possibleTypes _ = possibleTypes (Proxy @f)

instance (UnionRep a t, UnionRep b t) => UnionRep (a :+: b) t where
  possibleTypes _ x = possibleTypes (Proxy @a) x ++ possibleTypes (Proxy @b) x

--
--  GENERIC OBJECT: INPUT and OUTPUT plus ARGUMENTS
--
resolveTypes :: DataTypeLib -> [TypeUpdater] -> SchemaValidation DataTypeLib
resolveTypes = foldM (&)

class ObjectRep rep t where
  objectFieldTypes :: Proxy rep -> [((Text, DataField t), TypeUpdater)]

instance ObjectRep f t => ObjectRep (M1 D x f) t where
  objectFieldTypes _ = objectFieldTypes (Proxy @f)

instance ObjectRep f t => ObjectRep (M1 C x f) t where
  objectFieldTypes _ = objectFieldTypes (Proxy @f)

instance (ObjectRep a t, ObjectRep b t) => ObjectRep (a :*: b) t where
  objectFieldTypes _ = objectFieldTypes (Proxy @a) ++ objectFieldTypes (Proxy @b)

instance ObjectRep U1 t where
  objectFieldTypes _ = []

-- class Types class
type GQL_TYPE a = (Generic a, GQLType a)

type EnumConstraint a = (GQL_TYPE a, EnumRep (Rep a))

type InputObjectConstraint a = (GQL_TYPE a, ObjectRep (Rep a) ())

type ObjectConstraint a = (GQL_TYPE a, ObjectRep (Rep a) DataArguments)

scalarTypeOf :: GQLType a => DataValidator -> Proxy a -> DataFullType
scalarTypeOf validator = Leaf . LeafScalar . buildType validator

enumTypeOf :: GQLType a => [Text] -> Proxy a -> DataFullType
enumTypeOf tags' = Leaf . LeafEnum . buildType tags'

type InputType = ()

type OutputType = DataArguments

type InputOf t = Context t (KIND t) InputType

type OutputOf t = Context t (KIND t) OutputType

introspectOutputType ::
     forall a. Introspect a (KIND a) OutputType
  => Proxy a
  -> TypeUpdater
introspectOutputType _ = introspect (Context :: OutputOf a)

-- | context , like Proxy with multiple parameters
-- contains types of :
-- * 'a': actual gql type
-- * 'kind': object, scalar, enum ...
-- * 'args': InputType | OutputType
data Context a kind args =
  Context

buildField :: GQLType a => DataTypeKind -> Proxy a -> t -> Text -> DataField t
buildField fieldKind proxy' fieldArgs fieldName =
  DataField
    { fieldName
    , fieldKind
    , fieldArgs
    , fieldTypeWrappers = [NonNullType]
    , fieldType = __typeName proxy'
    , fieldHidden = False
    }

buildType :: GQLType a => t -> Proxy a -> DataType t
buildType typeData proxy =
  DataType
    { typeName = __typeName proxy
    , typeFingerprint = __typeFingerprint proxy
    , typeDescription = description proxy
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

-- |   Generates internal GraphQL Schema for query validation and introspection rendering
-- * 'kind': object, scalar, enum ...
-- * 'args': type of field arguments
--    * '()' for 'input values' , they are just JSON properties and does not have any argument
--    * 'DataArguments' for field Resolvers Types, where 'DataArguments' is type of arguments
class Introspect a kind args where
  __field :: Context a kind args -> Text -> DataField args
    --   generates data field representation of object field
    --   according to parameter 'args' it could be
    --   * input object field: if args is '()'
    --   * object: if args is 'DataArguments'
  introspect :: Context a kind args -> TypeUpdater -- Generates internal GraphQL Schema

type OutputConstraint a = Introspect a (KIND a) DataArguments

--
-- SCALAR
--
instance (GQLScalar a, GQLType a) => Introspect a SCALAR InputType where
  __field _ = buildField KindScalar (Proxy @a) ()
  introspect _ = updateLib (scalarTypeOf (scalarValidator $ Proxy @a)) [] (Proxy @a)

instance (GQLScalar a, GQLType a) => Introspect a SCALAR OutputType where
  __field _ = buildField KindScalar (Proxy @a) []
  introspect _ = updateLib (scalarTypeOf (scalarValidator $ Proxy @a)) [] (Proxy @a)

--
-- ENUM
--
instance EnumConstraint a => Introspect a ENUM InputType where
  __field _ = buildField KindEnum (Proxy @a) ()
  introspect _ = introspectEnum (Context :: InputOf a)

instance EnumConstraint a => Introspect a ENUM OutputType where
  __field _ = buildField KindEnum (Proxy @a) []
  introspect _ = introspectEnum (Context :: OutputOf a)

introspectEnum ::
     forall a f. (GQLType a, EnumRep (Rep a))
  => Context a (KIND a) f
  -> TypeUpdater
introspectEnum _ = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) [] (Proxy @a)

--
-- OBJECTS , INPUT_OBJECT
--
instance InputObjectConstraint a => Introspect a INPUT_OBJECT InputType where
  __field _ = buildField KindInputObject (Proxy @a) ()
  introspect _ = updateLib (InputObject . buildType fields') stack' (Proxy @a)
    where
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

instance ObjectConstraint a => Introspect a OBJECT OutputType where
  __field _ = buildField KindObject (Proxy @a) []
  introspect _ = updateLib (OutputObject . buildType (__typename : fields')) stack' (Proxy @a)
    where
      __typename =
        ( "__typename"
        , DataField
            { fieldName = "__typename"
            , fieldKind = KindScalar
            , fieldArgs = []
            , fieldTypeWrappers = []
            , fieldType = "String"
            , fieldHidden = True
            })
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
-- iterates on field types  and introspects them recursively
instance (Selector s, Introspect a (KIND a) f) => ObjectRep (RecSel s a) f where
  objectFieldTypes _ =
    [((name, __field (Context :: Context a (KIND a) f) name), introspect (Context :: Context a (KIND a) f))]
    where
      name = pack $ selName (undefined :: SelOf s)

--
-- UNION
--
-- | recursion for union types
-- iterates on possible types for UNION and introspects them recursively
instance (OutputConstraint a, ObjectConstraint a) => UnionRep (RecSel s a) OutputType where
  possibleTypes _ _ = [(buildField KindObject (Proxy @a) () "", introspect (Context :: OutputOf a))]

instance (GQL_TYPE a, UnionRep (Rep a) OutputType) => Introspect a UNION OutputType where
  __field _ = buildField KindUnion (Proxy @a) []
  introspect _ = updateLib (Union . buildType fields) stack (Proxy @a)
    where
      (fields, stack) = unzip $ possibleTypes (Proxy @(Rep a)) (Proxy @OutputType)

--
-- INPUT_UNION
--
instance (GQL_TYPE a, Introspect a INPUT_OBJECT InputType) => UnionRep (RecSel s a) InputType where
  possibleTypes _ _ =
    [ ( maybeField $ buildField KindInputObject (Proxy @a) () (__typeName $ Proxy @a)
      , introspect (Context :: Context a INPUT_OBJECT InputType))
    ]

instance (GQL_TYPE a, UnionRep (Rep a) InputType) => Introspect a INPUT_UNION InputType where
  __field _ = buildField KindInputUnion (Proxy @a) ()
  introspect _ = updateLib (InputUnion . buildType (fieldTag : fields)) (tagsEnumType : stack) (Proxy @a)
    where
      (fields, stack) = unzip $ possibleTypes (Proxy @(Rep a)) (Proxy @InputType)
      -- for every input Union 'User' adds enum type of possible TypeNames 'UserTags'
      tagsEnumType :: TypeUpdater
      tagsEnumType x = pure $ defineType (enumTypeName, Leaf $ LeafEnum tagsEnum) x
        where
          tagsEnum =
            DataType
              { typeName = enumTypeName
              -- has same fingerprint as object because it depends on it
              , typeFingerprint = __typeFingerprint (Proxy @a)
              , typeDescription = ""
              , typeData = map fieldName fields
              }
      enumTypeName = __typeName (Proxy @a) <> "Tags"
      fieldTag =
        DataField
          { fieldName = "tag"
          , fieldKind = KindEnum
          , fieldArgs = ()
          , fieldTypeWrappers = [NonNullType]
          , fieldType = enumTypeName
          , fieldHidden = True
          }

--
-- WRAPPER : Maybe, LIST , Resolver
--
maybeField :: DataField f -> DataField f
maybeField field@DataField {fieldTypeWrappers = NonNullType:xs} = field {fieldTypeWrappers = xs}
maybeField field                                                = field

instance Introspect a (KIND a) f => Introspect (Maybe a) WRAPPER f where
  __field _ name = maybeField $ __field (Context :: Context a (KIND a) f) name
  introspect _ = introspect (Context :: Context a (KIND a) f)

instance Introspect a (KIND a) f => Introspect [a] WRAPPER f where
  __field _ name = listField (__field (Context :: Context a (KIND a) f) name)
    where
      listField :: DataField f -> DataField f
      listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}
  introspect _ = introspect (Context :: Context a (KIND a) f)

--
-- CUSTOM Types: Tuple, Map, Set
--
instance Introspect (Pair k v) OBJECT f => Introspect (k, v) WRAPPER f where
  __field _ = __field (Context :: Context (Pair k v) OBJECT f)
  introspect _ = introspect (Context :: Context (Pair k v) OBJECT f)

instance Introspect [a] WRAPPER f => Introspect (Set a) WRAPPER f where
  __field _ = __field (Context :: Context [a] WRAPPER f)
  introspect _ = introspect (Context :: Context [a] WRAPPER f)

-- | introspection Does not care about resolving monad, some fake monad just for mocking
type MockRes = (Resolver Maybe)

instance Introspect (MapKind k v MockRes) OBJECT f => Introspect (Map k v) WRAPPER f where
  __field _ = __field (Context :: Context (MapKind k v MockRes) OBJECT f)
  introspect _ = introspect (Context :: Context (MapKind k v MockRes) OBJECT f)

-- |introspects Of Resolver 'a' as argument and 'b' as output type
instance (ObjectRep (Rep a) (), OutputConstraint b) => Introspect (a -> Resolver m b) WRAPPER OutputType where
  __field _ name = (__field (Context :: OutputOf b) name) {fieldArgs = map fst $ objectFieldTypes (Proxy @(Rep a))}
  introspect _ typeLib = resolveTypes typeLib $ map snd args ++ [introspect (Context :: OutputOf b)]
    where
      args :: [((Text, DataInputField), TypeUpdater)]
      args = objectFieldTypes (Proxy @(Rep a))

instance (ObjectRep (Rep a) (), OutputConstraint b) => Introspect (a -> Either String b) WRAPPER OutputType where
  __field _ name = (__field (Context :: OutputOf b) name) {fieldArgs = map fst $ objectFieldTypes (Proxy @(Rep a))}
  introspect _ typeLib = resolveTypes typeLib $ map snd args ++ [introspect (Context :: OutputOf b)]
    where
      args :: [((Text, DataInputField), TypeUpdater)]
      args = objectFieldTypes (Proxy @(Rep a))

instance (ObjectRep (Rep a) (), OutputConstraint b) => Introspect (a -> (c, v -> Resolver m b)) WRAPPER OutputType where
  __field _ name = (__field (Context :: OutputOf b) name) {fieldArgs = map fst $ objectFieldTypes (Proxy @(Rep a))}
  introspect _ typeLib = resolveTypes typeLib $ map snd args ++ [introspect (Context :: OutputOf b)]
    where
      args :: [((Text, DataInputField), TypeUpdater)]
      args = objectFieldTypes (Proxy @(Rep a))
