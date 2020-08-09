{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Validation.Query.UnionSelection
  ( validateUnionSelection,
  )
where

import Control.Monad ((>=>))
-- MORPHEUS
import Data.Morpheus.Error.Selection (unknownSelectionField)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
    empty,
    fromElems,
    selectOr,
    singleton,
  )
import Data.Morpheus.Types.Internal.AST
  ( DataUnion,
    FieldsDefinition,
    Fragment (..),
    OUT,
    RAW,
    Ref (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    SelectionSet,
    TypeDefinition (..),
    UnionMember (..),
    UnionTag (..),
    VALID,
  )
import qualified Data.Morpheus.Types.Internal.AST.MergeSet as MS
  ( join,
  )
import Data.Morpheus.Types.Internal.Validation
  ( FragmentValidator,
    Scope (..),
    SelectionValidator,
    askTypeMember,
    asksScope,
  )
import Data.Morpheus.Validation.Query.Fragment
  ( castFragmentType,
    resolveSpread,
  )

type TypeDef =
  ( TypeDefinition OUT VALID,
    FieldsDefinition OUT VALID
  )

-- returns all Fragments used in Union
exploreUnionFragments ::
  [UnionMember OUT s] ->
  Selection RAW ->
  FragmentValidator s' [Fragment VALID]
exploreUnionFragments unionTags = splitFrag
  where
    splitFrag ::
      Selection RAW -> FragmentValidator s [Fragment VALID]
    --  splitFrag (Spread _ ref) = pure <$> resolveSpread (map memberName unionTags) ref
    splitFrag Selection {selectionName = "__typename", selectionContent = SelectionField} = pure []
    splitFrag Selection {selectionName, selectionPosition} = do
      typeName <- asksScope currentTypeName
      failure $ unknownSelectionField typeName (Ref selectionName selectionPosition)

-- splitFrag (InlineFragment fragment) =
--   packFragment
--     <$> castFragmentType Nothing (fragmentPosition fragment) (map memberName unionTags) fragment

-- sorts Fragment by contitional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments ::
  [TypeDef] ->
  [Fragment VALID] ->
  [(TypeDef, [Fragment VALID])]
tagUnionFragments types fragments =
  filter notEmpty (map categorizeType types)
  where
    notEmpty = not . null . snd
    categorizeType ::
      TypeDef ->
      (TypeDef, [Fragment VALID])
    categorizeType datatype@(TypeDefinition {typeName}, _) = (datatype, filter matches fragments)
      where
        matches fragment = fragmentType fragment == typeName

{-
    - all Variable and Fragment references will be: resolved and validated
    - unionTypes: will be clustered under type names
      ...A on T1 {<SelectionA>}
      ...B on T2 {<SelectionB>}
      ...C on T2 {<SelectionC>}
      will be become : [
          UnionTag "T1" {<SelectionA>},
          UnionTag "T2" {<SelectionB>,<SelectionC>}
      ]
 -}
validateCluster ::
  forall s.
  ( TypeDef ->
    SelectionSet RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  SelectionSet RAW ->
  [(TypeDef, [Fragment VALID])] ->
  FragmentValidator s (SelectionContent VALID)
validateCluster validator __typenameSet =
  traverse _validateCluster
    >=> fmap UnionSelection . fromElems
  where
    _validateCluster :: (TypeDef, [Fragment VALID]) -> FragmentValidator s UnionTag
    _validateCluster (unionType@(TypeDefinition {typeName}, _), fragmets) = do
      selSet <- validator unionType __typenameSet
      UnionTag typeName <$> MS.join (selSet : map fragmentSelection fragmets)

validateUnionSelection ::
  (TypeDef -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  SelectionSet RAW ->
  DataUnion VALID ->
  FragmentValidator s (SelectionContent VALID)
validateUnionSelection validate selectionSet members = do
  let (__typename :: SelectionSet RAW) = selectOr empty singleton "__typename" selectionSet
  -- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
  -- [("User", FieldsDefinition { ... }), ("Product", FieldsDefinition { ...
  unionTypes <- traverse askTypeMember members
  -- find all Fragments used in Selection
  spreads <- concat <$> traverse (exploreUnionFragments members) (elems selectionSet)
  let categories = tagUnionFragments unionTypes spreads
  validateCluster validate __typename categories
