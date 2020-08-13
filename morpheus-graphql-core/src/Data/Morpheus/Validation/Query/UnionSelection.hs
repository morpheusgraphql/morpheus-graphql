{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Morpheus.Validation.Query.UnionSelection
  ( validateUnionSelection,
    validateInterfaceSelection,
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
    Fragment (..),
    IMPLEMENTABLE,
    OUT,
    RAW,
    Ref (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    SelectionSet,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    UnionMember (..),
    UnionTag (..),
    VALID,
    toCategory,
  )
import qualified Data.Morpheus.Types.Internal.AST.MergeSet as MS
  ( join,
  )
import Data.Morpheus.Types.Internal.Validation
  ( FragmentValidator,
    Scope (..),
    askInterfaceTypes,
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

-- returns all Fragments used in Union
exploreInterfaceFragments ::
  (ResolveFragment s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  TypeDefinition IMPLEMENTABLE VALID ->
  [TypeDefinition IMPLEMENTABLE VALID] ->
  Selection RAW ->
  FragmentValidator s ([UnionTag], [Selection RAW])
exploreInterfaceFragments _ _ _ x@Selection {} = pure ([], [x])
exploreInterfaceFragments f t types (Spread _ ref) = pureFirst <$> resolveValidFragment f (typeName <$> t : types) ref
exploreInterfaceFragments f t types (InlineFragment fragment@Fragment {fragmentType}) =
  pureFirst . UnionTag fragmentType
    <$> ( castFragmentType Nothing (fragmentPosition fragment) (typeName <$> t : types) fragment
            >>= f
        )

pureFirst :: a1 -> ([a1], [a2])
pureFirst x = ([x], [])

joinExploredSelection :: [([UnionTag], [Selection RAW])] -> FragmentValidator s ([UnionTag], SelectionSet RAW)
joinExploredSelection i = do
  let (x, y) = unzip i
  let x' = concat x
  let y' = concat y
  (x',) <$> fromElems y'

-- sorts Fragment by contitional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments ::
  [TypeDefinition IMPLEMENTABLE VALID] ->
  [UnionTag] ->
  [(TypeDefinition IMPLEMENTABLE VALID, [SelectionSet VALID])]
tagUnionFragments types fragments =
  filter notEmpty (map categorizeType types)
  where
    notEmpty = not . null . snd
    categorizeType ::
      TypeDefinition IMPLEMENTABLE VALID ->
      (TypeDefinition IMPLEMENTABLE VALID, [SelectionSet VALID])
    categorizeType datatype = (datatype, unionTagSelection <$> filter matches fragments)
      where
        matches = (`elem` subTypes datatype) . unionTagName

subTypes :: TypeDefinition a s -> [TypeName]
subTypes TypeDefinition {typeName, typeContent = DataObject {objectImplements}} =
  typeName : objectImplements
subTypes TypeDefinition {typeName} = [typeName]

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
  ( TypeDefinition IMPLEMENTABLE VALID ->
    SelectionSet RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  SelectionSet RAW ->
  [(TypeDefinition IMPLEMENTABLE VALID, [SelectionSet VALID])] ->
  FragmentValidator s (SelectionContent VALID)
validateCluster validator regularSelection =
  traverse _validateCluster
    >=> fmap UnionSelection . fromElems
  where
    _validateCluster :: (TypeDefinition IMPLEMENTABLE VALID, [SelectionSet VALID]) -> FragmentValidator s UnionTag
    _validateCluster (unionType@TypeDefinition {typeName}, fragmets) = do
      selSet <- validator unionType regularSelection
      UnionTag typeName <$> MS.join (selSet : fragmets)

validateFragmentCluster ::
  SelectionSet VALID ->
  [(TypeDefinition IMPLEMENTABLE VALID, [SelectionSet VALID])] ->
  FragmentValidator s (SelectionContent VALID)
validateFragmentCluster regularSelection =
  traverse _validateCluster
    >=> fmap UnionSelection . fromElems
  where
    _validateCluster :: (TypeDefinition IMPLEMENTABLE VALID, [SelectionSet VALID]) -> FragmentValidator s UnionTag
    _validateCluster (TypeDefinition {typeName}, fragmets) =
      UnionTag typeName <$> MS.join (regularSelection : fragmets)

validateInterfaceSelection ::
  ResolveFragment s =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition IMPLEMENTABLE VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  TypeDefinition IMPLEMENTABLE VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateInterfaceSelection
  validateFragment
  validate
  typeDef
  inputSelectionSet = do
    possibleTypes <- askInterfaceTypes typeDef
    (spreads, selectionSet) <-
      traverse
        (exploreInterfaceFragments validateFragment typeDef possibleTypes)
        (elems inputSelectionSet)
        >>= joinExploredSelection
    validSelectionSet <- validate typeDef selectionSet
    let categories = tagUnionFragments (typeDef : possibleTypes) spreads
    if null categories
      then pure (SelectionSet validSelectionSet)
      else validateFragmentCluster validSelectionSet categories

validateUnionSelection ::
  ResolveFragment s =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition IMPLEMENTABLE VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  DataUnion VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateUnionSelection validateFragment validate members selectionSet = do
  let (__typename :: SelectionSet RAW) = selectOr empty singleton "__typename" selectionSet
  -- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
  -- [("User", FieldsDefinition { ... }), ("Product", FieldsDefinition { ...
  unionTypes <- traverse (fmap toCategory . askTypeMember) members
  -- find all Fragments used in Selection
  spreads <- concat <$> traverse (exploreUnionFragments validateFragment members) (elems selectionSet)
  let categories = tagUnionFragments unionTypes spreads
  validateCluster validate __typename categories
