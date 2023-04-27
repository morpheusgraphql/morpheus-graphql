{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Internal.Schema.Union
  ( buildUnionType,
  )
where

import Data.List (partition)
import Data.Morpheus.Generic
  ( GRepCons (..),
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils (fromElems)
import Data.Morpheus.Server.Deriving.Internal.Schema.Enum
  (
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Object
  ( defineObjectType,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( NodeDerivation (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Types (NodeTypeVariant (..))
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    TRUE,
    TypeContent (..),
    TypeName,
    mkNullaryMember,
    mkUnionMember,
  )
import Relude

buildUnionType ::
  CatType kind a ->
  [TypeName] ->
  [GRepCons (Maybe (ArgumentsDefinition CONST))] ->
  GQLResult (TypeContent TRUE kind CONST, [NodeDerivation])
buildUnionType p@InputType variantRefs inlineVariants = do
  let nodes = [UnionType ([NodeUnitType | not (null nullaryVariants)] <> typeDependencies p objectVariants)]
  variants <- fromElems members
  pure (DataInputUnion variants, nodes)
  where
    (nullaryVariants, objectVariants) = partition null inlineVariants
    members =
      map mkUnionMember (variantRefs <> map consName objectVariants)
        <> fmap (mkNullaryMember . consName) nullaryVariants
buildUnionType p@OutputType unionRef unionCons = do
  variants <- fromElems (map mkUnionMember (unionRef <> map consName unionCons))
  pure (DataUnion variants, [UnionType (typeDependencies p unionCons)])

typeDependencies :: CatType kind a -> [GRepCons (Maybe (ArgumentsDefinition CONST))] -> [NodeTypeVariant]
typeDependencies proxy cons = concat $ traverse (defineObjectType proxy) cons
