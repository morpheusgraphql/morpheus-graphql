{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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
    SelectionDefinition (..),
    SelectionSet,
    SelectionSet,
    TypeName,
    UnionTag (..),
    VALID,
  )
import qualified Data.Morpheus.Types.Internal.AST.MergeSet as MS
  ( join,
  )
import Data.Morpheus.Types.Internal.Validation
  ( SelectionValidator,
    askScopeTypeName,
    askTypeMember,
  )
import Data.Morpheus.Validation.Query.Fragment
  ( castFragmentType,
    resolveSpread,
  )

type TypeDef = (TypeName, FieldsDefinition OUT)

-- returns all Fragments used in Union
exploreUnionFragments ::
  [TypeName] ->
  Selection RAW ->
  SelectionValidator [Fragment]
exploreUnionFragments unionTags = splitFrag
  where
    packFragment fragment = [fragment]
    splitFrag ::
      Selection RAW -> SelectionValidator [Fragment]
    splitFrag (Spread _ ref) = packFragment <$> resolveSpread unionTags ref
    splitFrag (Selection SelectionDefinition {selectionName = "__typename", selectionContent = SelectionField}) = pure []
    splitFrag (Selection SelectionDefinition {selectionName, selectionPosition}) = do
      typeName <- askScopeTypeName
      failure $ unknownSelectionField typeName (Ref selectionName selectionPosition)
    splitFrag (InlineFragment fragment) =
      packFragment
        <$> castFragmentType Nothing (fragmentPosition fragment) unionTags fragment

-- sorts Fragment by contitional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments ::
  [TypeDef] -> [Fragment] -> [(TypeDef, [Fragment])]
tagUnionFragments types fragments =
  filter notEmpty $
    map categorizeType types
  where
    notEmpty = not . null . snd
    categorizeType :: (TypeName, FieldsDefinition OUT) -> (TypeDef, [Fragment])
    categorizeType datatype = (datatype, filter matches fragments)
      where
        matches fragment = fragmentType fragment == fst datatype

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
  (TypeDef -> SelectionSet RAW -> SelectionValidator (SelectionSet VALID)) ->
  SelectionSet RAW ->
  [(TypeDef, [Fragment])] ->
  SelectionValidator (SelectionContent VALID)
validateCluster validator __typename = traverse _validateCluster >=> fmap UnionSelection . fromElems
  where
    _validateCluster :: (TypeDef, [Fragment]) -> SelectionValidator UnionTag
    _validateCluster (unionType, fragmets) = do
      fragmentSelections <- MS.join (__typename : map fragmentSelection fragmets)
      UnionTag (fst unionType) <$> validator unionType fragmentSelections

validateUnionSelection ::
  (TypeDef -> SelectionSet RAW -> SelectionValidator (SelectionSet VALID)) ->
  SelectionSet RAW ->
  DataUnion ->
  SelectionValidator (SelectionContent VALID)
validateUnionSelection validate selectionSet members = do
  let (__typename :: SelectionSet RAW) = selectOr empty singleton "__typename" selectionSet
  -- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
  -- [("User", FieldsDefinition { ... }), ("Product", FieldsDefinition { ...
  unionTypes <- traverse askTypeMember members
  -- find all Fragments used in Selection
  spreads <- concat <$> traverse (exploreUnionFragments members) (elems selectionSet)
  let categories = tagUnionFragments unionTypes spreads
  validateCluster validate __typename categories
