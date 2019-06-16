{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Resolve.Internal where

import           Data.Morpheus.Resolve.Generics.DeriveResolvers (ObjectFieldResolvers, UnionResolvers)
import           Data.Morpheus.Resolve.Generics.EnumRep         (EnumRep (..))
import           Data.Morpheus.Resolve.Generics.TypeRep         (ObjectRep (..), UnionRep (..))
import           Data.Morpheus.Types.GQLType                    (GQLType (..))
import           Data.Morpheus.Types.Internal.Data              (DataArguments)
import           GHC.Generics

-- class Types class
type GQL_TYPE a = (Generic a, GQLType a)

type EnumConstraint a = (GQL_TYPE a, EnumRep (Rep a))

type InputObjectRep a = ObjectRep (Rep a) ()

type InputObjectConstraint a = (GQL_TYPE a, InputObjectRep a)

type ObjectConstraint a = (GQL_TYPE a, ObjectRep (Rep a) DataArguments)

type EncodeObjectConstraint a b = (ObjectFieldResolvers (Rep a) b, ObjectConstraint a)

type UnionConstraint a = (GQL_TYPE a, UnionRep (Rep a))

type EncodeUnionConstraint a res = (UnionResolvers (Rep a) res, UnionConstraint a)
