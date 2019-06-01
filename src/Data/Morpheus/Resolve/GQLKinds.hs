{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Morpheus.Resolve.GQLKinds where

import           Data.Morpheus.Resolve.Generics.DeriveResolvers (DeriveResolvers)
import           Data.Morpheus.Resolve.Generics.EnumRep         (EnumRep (..))
import           Data.Morpheus.Resolve.Generics.GDecode         (GDecode (..))
import           Data.Morpheus.Resolve.Generics.ObjectRep       (ObjectRep (..))
import           Data.Morpheus.Resolve.Generics.UnionRep        (UnionRep (..))
import           Data.Morpheus.Resolve.Generics.UnionResolvers  (UnionResolvers (..))
import           Data.Morpheus.Types.GQLType                    (GQLType (..), asObjectType, enumTypeOf, inputObjectOf)
import           Data.Morpheus.Types.Internal.AST.Selection     (Selection (..))
import           Data.Morpheus.Types.Internal.Data              (DataFullType (..), DataInputField, DataOutputField,
                                                                 DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation        (ResolveIO, Validation)
import           Data.Morpheus.Types.Internal.Value             (Value (..))
import           Data.Proxy                                     (Proxy (..))
import           Data.Text                                      (Text)
import           GHC.Generics

-- class Types class
type Intro_ a = Proxy a -> DataTypeLib -> DataTypeLib

type Decode_ a = Value -> Validation a

type Encode_ a b = (Text, Selection) -> a -> ResolveIO b

type IField_ a = Proxy a -> Text -> DataInputField

type OField_ a = Proxy a -> Text -> DataOutputField

type EnumConstraint a = (Generic a, EnumRep (Rep a), GQLType a)

introspectEnum ::
     forall a. (GQLType a, EnumRep (Rep a))
  => Intro_ a
introspectEnum = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

{-
 INPUT Object
-}
type IOObjectRep a = ObjectRep (Rep a) (Text, DataInputField)

type IObjectConstraint a = (GQLType a, Generic a, GDecode Value (Rep a), IOObjectRep a)

introspectInputObject ::
     forall a. (GQLType a, IOObjectRep a)
  => Intro_ a
introspectInputObject = updateLib (inputObjectOf fields') stack'
  where
    (fields', stack') = unzip $ getFields (Proxy @(Rep a))

{-

OBJECT

-}
type EncodeObjectConstraint a b = (DeriveResolvers (Rep a) b, ObjectConstraint a)

type ObjectConstraint a = (Generic a, ObjectRep (Rep a) (Text, DataOutputField), GQLType a)

introspectObject ::
     forall a. (ObjectRep (Rep a) (Text, DataOutputField), GQLType a)
  => Intro_ a
introspectObject = updateLib (asObjectType fields') stack'
  where
    (fields', stack') = unzip $ getFields (Proxy @(Rep a))

{-

UNION

-}
type EncodeUnionConstraint a res = (UnionResolvers (Rep a) res, UnionConstraint a)

type UnionConstraint a = (Generic a, GQLType a, UnionRep (Rep a))

introspectUnion ::
     forall a. (GQLType a, UnionRep (Rep a))
  => Intro_ a
introspectUnion = updateLib (const $ Union fields) stack
  where
    (fields, stack) = unzip $ possibleTypes (Proxy @(Rep a))
