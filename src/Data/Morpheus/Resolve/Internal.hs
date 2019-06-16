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
import           Data.Morpheus.Types.Internal.Data              (DataArguments)
import           Data.Morpheus.Types.Internal.Validation        (Validation)
import           Data.Morpheus.Types.Internal.Value             (Value (..))
import           GHC.Generics

type InputType = ()

type OutputType = DataArguments

type InputOf t = Context t (KIND t) InputType

type OutputOf t = Context t (KIND t) OutputType

-- | context , like Proxy with multiple parameters
-- contains types of :
-- * 'a': actual gql type
-- * 'kind': object, scalar, enum ...
-- * 'args': InputType | OutputType
data Context a kind args =
  Context

-- class Types class
type Intro_ a b c = Context a b c -> TypeUpdater

type Decode_ a = Value -> Validation a

type GQL_TYPE a = (Generic a, GQLType a)

type EnumConstraint a = (GQL_TYPE a, EnumRep (Rep a))

type InputObjectRep a = ObjectRep (Rep a) ()

type InputObjectConstraint a = (GQL_TYPE a, InputObjectRep a)

type ObjectConstraint a = (GQL_TYPE a, ObjectRep (Rep a) DataArguments)

type EncodeObjectConstraint a b = (DeriveResolvers (Rep a) b, ObjectConstraint a)

type UnionConstraint a = (GQL_TYPE a, UnionRep (Rep a))

type EncodeUnionConstraint a res = (UnionResolvers (Rep a) res, UnionConstraint a)
