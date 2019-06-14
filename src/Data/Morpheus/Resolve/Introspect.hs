{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Resolve.Introspect where

import           Data.Morpheus.Kind                     (ENUM, INPUT_OBJECT, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Resolve.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Resolve.Generics.TypeRep (ObjectRep (..), RecSel, SelOf, UnionRep (..), resolveTypes)
import           Data.Morpheus.Resolve.Internal         (CX (..), EnumConstraint, InputObjectConstraint, InputOf,
                                                         Intro_, ObjectConstraint, OutputOf, UnionConstraint)
import           Data.Morpheus.Schema.Type              (DeprecationArgs)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import qualified Data.Morpheus.Types.GQLArgs            as Args (GQLArgs (..))
import           Data.Morpheus.Types.GQLScalar          (GQLScalar (..))
import           Data.Morpheus.Types.GQLType            (GQLType (..))
import           Data.Morpheus.Types.Internal.Data      (DataArguments, DataField (..), DataFullType (..),
                                                         DataLeaf (..), DataTypeWrapper (..), DataValidator)
import           Data.Morpheus.Types.Resolver           (Resolver (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text, pack)
import           GHC.Generics

scalarTypeOf :: GQLType a => DataValidator -> Proxy a -> DataFullType
scalarTypeOf validator = Leaf . LeafScalar . buildType validator

enumTypeOf :: GQLType a => [Text] -> Proxy a -> DataFullType
enumTypeOf tags' = Leaf . LeafEnum . buildType tags'

class Introspect a kind f where
  _field :: CX a kind f -> Text -> DataField f
  _introspect :: Intro_ a kind f

type OutputConstraint a = Introspect a (KIND a) DataArguments

type InputConstraint a = Introspect a (KIND a) ()

{--

  Introspect SCALAR Types: SCALAR, ENUM

-}
introspectEnum ::
     forall a f. (GQLType a, EnumRep (Rep a))
  => Intro_ a (KIND a) f
introspectEnum _ = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) [] (Proxy @a)

instance (GQLScalar a, GQLType a) => Introspect a SCALAR DataArguments where
  _field _ = field_ SCALAR (Proxy @a) []
  _introspect _ = updateLib (scalarTypeOf (scalarValidator $ Proxy @a)) [] (Proxy @a)

instance EnumConstraint a => Introspect a ENUM DataArguments where
  _field _ = field_ ENUM (Proxy @a) []
  _introspect _ = introspectEnum (CX :: OutputOf a)

instance (GQLScalar a, GQLType a) => Introspect a SCALAR () where
  _field _ = field_ SCALAR (Proxy @a) ()
  _introspect _ = updateLib (scalarTypeOf (scalarValidator $ Proxy @a)) [] (Proxy @a)

instance EnumConstraint a => Introspect a ENUM () where
  _field _ = field_ ENUM (Proxy @a) ()
  _introspect _ = introspectEnum (CX :: InputOf a)

{--

  Introspect OBJECT Types:  OBJECTS , INPUT_OBJECT

-}
instance ObjectConstraint a => Introspect a OBJECT DataArguments where
  _field _ = field_ OBJECT (Proxy @a) []
  _introspect _ = updateLib (OutputObject . buildType fields') stack' (Proxy @a)
    where
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

instance InputObjectConstraint a => Introspect a INPUT_OBJECT () where
  _field _ = field_ INPUT_OBJECT (Proxy @a) ()
  _introspect _ = updateLib (InputObject . buildType fields') stack' (Proxy @a)
    where
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

instance (Selector s, Introspect a (KIND a) f) => ObjectRep (RecSel s a) f where
  objectFieldTypes _ = [((name, _field (CX :: CX a (KIND a) f) name), _introspect (CX :: CX a (KIND a) f))]
    where
      name = pack $ selName (undefined :: SelOf s)

{--

  Introspect UNION Types:  UNION

-}
instance (OutputConstraint a, ObjectConstraint a) => UnionRep (RecSel s a) where
  possibleTypes _ = [(field_ OBJECT (Proxy @a) () "", _introspect (CX :: OutputOf a))]

instance UnionConstraint a => Introspect a UNION DataArguments where
  _field _ = field_ UNION (Proxy @a) []
  _introspect _ = updateLib (Union . buildType fields) stack (Proxy @a)
    where
      (fields, stack) = unzip $ possibleTypes (Proxy @(Rep a))

{--

  Introspect WRAPPER Types: Maybe, LIST , Resolver

-}
instance Introspect a (KIND a) f => Introspect (Maybe a) WRAPPER f where
  _introspect _ = _introspect (CX :: CX a (KIND a) f)
  _field _ name = maybeField $ _field (CX :: CX a (KIND a) f) name
    where
      maybeField :: DataField f -> DataField f
      maybeField field@DataField {fieldTypeWrappers = NonNullType:xs} = field {fieldTypeWrappers = xs}
      maybeField field                                                = field

instance Introspect a (KIND a) f => Introspect [a] WRAPPER f where
  _introspect _ = _introspect (CX :: CX a (KIND a) f)
  _field _ name = listField (_field (CX :: CX a (KIND a) f) name)
    where
      listField :: DataField f -> DataField f
      listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}

instance (OutputConstraint a, Args.GQLArgs p) => Introspect (Resolver c p a) WRAPPER DataArguments where
  _introspect _ typeLib = resolveTypes typeLib $ inputTypes' ++ [_introspect (CX :: OutputOf a)]
    where
      inputTypes' = map snd $ Args.introspect (Proxy @p)
  _field _ name = (_field (CX :: OutputOf a) name) {fieldArgs = map fst $ Args.introspect (Proxy @p)}

instance Args.GQLArgs DeprecationArgs
