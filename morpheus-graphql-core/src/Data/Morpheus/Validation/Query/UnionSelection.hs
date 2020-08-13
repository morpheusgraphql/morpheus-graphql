{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Morpheus.Validation.Query.UnionSelection
  ( validateUnionSelection,
    validateInterfaceSelection,
  )
where

import Control.Monad ((>=>))
-- MORPHEUS
import Data.Morpheus.Internal.Utils
  ( elems,
    empty,
    fromElems,
  )
import Data.Morpheus.Types.Internal.AST
  ( DataUnion,
    Fragment (..),
    IMPLEMENTABLE,
    RAW,
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    SelectionSet,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    UnionTag (..),
    VALID,
    mkType,
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

-- returns all Fragments used for Possible Types
splitFragment ::
  (ResolveFragment s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  [TypeDefinition IMPLEMENTABLE VALID] ->
  Selection RAW ->
  FragmentValidator s ([UnionTag], [Selection RAW])
splitFragment _ _ x@Selection {} = pure ([], [x])
splitFragment f types (Spread _ ref) = pureFirst <$> resolveValidFragment f (typeName <$> types) ref
splitFragment f types (InlineFragment fragment@Fragment {fragmentType}) =
  pureFirst . UnionTag fragmentType
    <$> ( castFragmentType Nothing (fragmentPosition fragment) (typeName <$> types) fragment
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

exploreFragments ::
  (ResolveFragment s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  [TypeDefinition IMPLEMENTABLE VALID] ->
  SelectionSet RAW ->
  FragmentValidator s ([UnionTag], SelectionSet RAW)
exploreFragments validateFragment types selectionSet =
  traverse (splitFragment validateFragment types) (elems selectionSet)
    >>= joinExploredSelection

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
joinCluster ::
  forall s.
  SelectionSet VALID ->
  [(TypeDefinition IMPLEMENTABLE VALID, [SelectionSet VALID])] ->
  FragmentValidator s (SelectionContent VALID)
joinCluster selSet =
  traverse _validateCluster
    >=> fmap UnionSelection . fromElems
  where
    _validateCluster :: (TypeDefinition IMPLEMENTABLE VALID, [SelectionSet VALID]) -> FragmentValidator s UnionTag
    _validateCluster (TypeDefinition {typeName}, fragmets) =
      UnionTag typeName <$> MS.join (selSet : fragmets)

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
    (spreads, selectionSet) <- exploreFragments validateFragment (typeDef : possibleTypes) inputSelectionSet
    validSelectionSet <- validate typeDef selectionSet
    let categories = tagUnionFragments (typeDef : possibleTypes) spreads
    if null categories
      then pure (SelectionSet validSelectionSet)
      else joinCluster validSelectionSet categories

mkUnionRootType :: FragmentValidator s (TypeDefinition IMPLEMENTABLE VALID)
mkUnionRootType = (`mkType` DataObject [] empty) <$> asksScope currentTypeName

validateUnionSelection ::
  ResolveFragment s =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition IMPLEMENTABLE VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  DataUnion VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateUnionSelection validateFragment validate members selectionSet = do
  -- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
  -- [("User", FieldsDefinition { ... }), ("Product", FieldsDefinition { ...
  unionTypes <- traverse (fmap toCategory . askTypeMember) members
  -- find all Fragments used in Selection
  (spreads, selSet) <- exploreFragments validateFragment unionTypes selectionSet
  typeDef <- mkUnionRootType
  validSelection <- validate typeDef selSet
  let categories = tagUnionFragments unionTypes spreads
  joinCluster validSelection categories
