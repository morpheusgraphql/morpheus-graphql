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
    OUTPUT_OBJECT,
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
    askTypeMember,
    asksScope,
  )
import Data.Morpheus.Validation.Query.Fragment
  ( ResolveFragment (resolveValidFragment),
    castFragmentType,
  )

-- returns all Fragments used in Union
exploreUnionFragments ::
  (ResolveFragment s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  [UnionMember OUT unionS] ->
  Selection RAW ->
  FragmentValidator s [UnionTag]
exploreUnionFragments f unionTags (Spread _ ref) = pure <$> resolveValidFragment f (map memberName unionTags) ref
exploreUnionFragments _ _ Selection {selectionName = "__typename", selectionContent = SelectionField} = pure []
exploreUnionFragments _ _ Selection {selectionName, selectionPosition} = do
  typeName <- asksScope currentTypeName
  failure $ unknownSelectionField typeName (Ref selectionName selectionPosition)
exploreUnionFragments f unionTags (InlineFragment fragment@Fragment {fragmentType}) =
  pure . UnionTag fragmentType
    <$> ( castFragmentType Nothing (fragmentPosition fragment) (map memberName unionTags) fragment
            >>= f
        )

-- sorts Fragment by contitional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments ::
  [TypeDefinition OUTPUT_OBJECT VALID] ->
  [UnionTag] ->
  [(TypeDefinition OUTPUT_OBJECT VALID, [UnionTag])]
tagUnionFragments types fragments =
  filter notEmpty (map categorizeType types)
  where
    notEmpty = not . null . snd
    categorizeType ::
      TypeDefinition OUTPUT_OBJECT VALID ->
      (TypeDefinition OUTPUT_OBJECT VALID, [UnionTag])
    categorizeType datatype@TypeDefinition {typeName} = (datatype, filter matches fragments)
      where
        matches = (typeName ==) . unionTagName

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
  ( TypeDefinition OUTPUT_OBJECT VALID ->
    SelectionSet RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  SelectionSet RAW ->
  [(TypeDefinition OUTPUT_OBJECT VALID, [UnionTag])] ->
  FragmentValidator s (SelectionContent VALID)
validateCluster validator __typenameSet =
  traverse _validateCluster
    >=> fmap UnionSelection . fromElems
  where
    _validateCluster :: (TypeDefinition OUTPUT_OBJECT VALID, [UnionTag]) -> FragmentValidator s UnionTag
    _validateCluster (unionType@TypeDefinition {typeName}, fragmets) = do
      selSet <- validator unionType __typenameSet
      UnionTag typeName <$> MS.join (selSet : map unionTagSelection fragmets)

validateUnionSelection ::
  ResolveFragment s =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition OUTPUT_OBJECT VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  SelectionSet RAW ->
  DataUnion VALID ->
  FragmentValidator s (SelectionContent VALID)
validateUnionSelection validateFragment validate selectionSet members = do
  let (__typename :: SelectionSet RAW) = selectOr empty singleton "__typename" selectionSet
  -- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
  -- [("User", FieldsDefinition { ... }), ("Product", FieldsDefinition { ...
  unionTypes <- traverse askTypeMember members
  -- find all Fragments used in Selection
  spreads <- concat <$> traverse (exploreUnionFragments validateFragment members) (elems selectionSet)
  let categories = tagUnionFragments unionTypes spreads
  validateCluster validate __typename categories
