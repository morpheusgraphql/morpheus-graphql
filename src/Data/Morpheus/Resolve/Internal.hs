{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Morpheus.Resolve.Internal where

import           Data.Morpheus.Resolve.Generics.DeriveResolvers (DeriveResolvers)
import           Data.Morpheus.Resolve.Generics.EnumRep         (EnumRep (..))
import           Data.Morpheus.Resolve.Generics.GDecode         (GDecode (..))
import           Data.Morpheus.Resolve.Generics.TypeRep         (ObjectRep (..), TypeUpdater, UnionRep (..))
import           Data.Morpheus.Resolve.Generics.UnionResolvers  (UnionResolvers (..))
import           Data.Morpheus.Types.GQLType                    (GQLType (..), enumTypeOf, inputObjectOf)
import           Data.Morpheus.Types.Internal.AST.Selection     (Selection (..))
import           Data.Morpheus.Types.Internal.Data              (DataField (..), DataInputField, DataOutputField,
                                                                 DataTypeWrapper (..))
import           Data.Morpheus.Types.Internal.Validation        (ResolveIO, Validation)
import           Data.Morpheus.Types.Internal.Value             (Value (..))
import           Data.Proxy                                     (Proxy (..))
import           Data.Text                                      (Text)
import           GHC.Generics

-- class Types class
type Intro_ a = Proxy a -> TypeUpdater

type Decode_ a = Value -> Validation a

type Encode_ a b = (Text, Selection) -> a -> ResolveIO b

type IField_ a = Proxy a -> Text -> DataInputField

type OField_ a = Proxy a -> Text -> DataOutputField

type GQL_TYPE a = (Generic a, GQLType a)

type EnumConstraint a = (GQL_TYPE a, EnumRep (Rep a))

type InputObjectRep a = ObjectRep (Rep a) (Text, DataInputField)

type InputObjectConstraint a = (GQL_TYPE a, GDecode Value (Rep a), InputObjectRep a)

type ObjectConstraint a = (GQL_TYPE a, ObjectRep (Rep a) (Text, DataOutputField))

type EncodeObjectConstraint a b = (DeriveResolvers (Rep a) b, ObjectConstraint a)

type UnionConstraint a = (GQL_TYPE a, UnionRep (Rep a))

type EncodeUnionConstraint a res = (UnionResolvers (Rep a) res, UnionConstraint a)

introspectEnum ::
     forall a. (GQLType a, EnumRep (Rep a))
  => Intro_ a
introspectEnum = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

introspectInputObject ::
     forall a. InputObjectConstraint a
  => Intro_ a
introspectInputObject = updateLib (inputObjectOf fields') stack'
  where
    (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

maybeField :: DataField a -> DataField a
maybeField field@DataField {fieldTypeWrappers = NonNullType:xs} = field {fieldTypeWrappers = xs}
maybeField field                                                = field

listField :: DataField a -> DataField a
listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}
