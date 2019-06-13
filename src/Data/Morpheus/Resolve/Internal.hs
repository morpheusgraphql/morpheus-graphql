{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Resolve.Internal where

import           Data.Morpheus.Kind                             (KIND)
import           Data.Morpheus.Resolve.Generics.DeriveResolvers (DeriveResolvers)
import           Data.Morpheus.Resolve.Generics.EnumRep         (EnumRep (..))
import           Data.Morpheus.Resolve.Generics.TypeRep         (ObjectRep (..), TypeUpdater, UnionRep (..))
import           Data.Morpheus.Resolve.Generics.UnionResolvers  (UnionResolvers (..))
import           Data.Morpheus.Types.GQLType                    (GQLType (..))
import           Data.Morpheus.Types.Internal.AST.Selection     (Selection (..))
import           Data.Morpheus.Types.Internal.Data              (DataArguments, DataInputField, DataOutputField)
import           Data.Morpheus.Types.Internal.Validation        (ResolveIO, Validation)
import           Data.Morpheus.Types.Internal.Value             (Value (..))
import           Data.Text                                      (Text)
import           GHC.Generics

type InputOf t = CX t (KIND t) ()

type OutputOf t = CX t (KIND t) DataArguments

data CX t k d =
  CX

-- class Types class
type Intro_ a b c = CX a b c -> TypeUpdater

type Decode_ a = Value -> Validation a

type Encode_ a b = (Text, Selection) -> a -> ResolveIO b

type IField_ a b = CX a b DataInputField -> Text -> DataInputField

type OField_ a b = CX a b DataOutputField -> Text -> DataOutputField

type GQL_TYPE a = (Generic a, GQLType a)

type EnumConstraint a = (GQL_TYPE a, EnumRep (Rep a))

type InputObjectRep a = ObjectRep (Rep a) ()

type InputObjectConstraint a = (GQL_TYPE a, InputObjectRep a)

type ObjectConstraint a = (GQL_TYPE a, ObjectRep (Rep a) DataArguments)

type EncodeObjectConstraint a b = (DeriveResolvers (Rep a) b, ObjectConstraint a)

type UnionConstraint a = (GQL_TYPE a, UnionRep (Rep a))

type EncodeUnionConstraint a res = (UnionResolvers (Rep a) res, UnionConstraint a)
