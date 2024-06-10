{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Query.UnionSelection
  ( validateUnionSelection,
    validateInterfaceSelection,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Mergeable (IsMap (toAssoc), OrdMap)
import Data.Mergeable.OrdMap (ordMapDelete)
import Data.Morpheus.Internal.Utils
  ( empty,
    fromElems,
    mergeConcat,
    selectOr,
    startHistory,
    unsafeFromList,
  )
import Data.Morpheus.Types.Internal.AST.DirectiveLocation (DirectiveLocation (..))
import Data.Morpheus.Types.Internal.AST.Name (TypeName)
import Data.Morpheus.Types.Internal.AST.Selection
  ( Fragment (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    UnionTag (..),
  )
import Data.Morpheus.Types.Internal.AST.Stage (RAW, VALID)
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( IMPLEMENTABLE,
    OUT,
    toCategory,
  )
import Data.Morpheus.Types.Internal.AST.TypeSystem
  ( TypeContent (..),
    TypeDefinition (..),
    UnionTypeDefinition,
    mkType,
  )
import Data.Morpheus.Types.Internal.Validation
  ( FragmentValidator,
    Scope (..),
    askInterfaceTypes,
    askTypeMember,
    asksScope,
  )
import Data.Morpheus.Validation.Internal.Directive (validateDirectives)
import Data.Morpheus.Validation.Query.Fragment
  ( ValidateFragmentSelection,
    castFragmentType,
    validateSpread,
  )
import Relude hiding (empty, join)

-- returns all Fragments used for Possible Types
splitFragment ::
  (ValidateFragmentSelection s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  [TypeDefinition IMPLEMENTABLE VALID] ->
  Selection RAW ->
  FragmentValidator s (Either UnionTag (Selection RAW))
splitFragment _ _ x@Selection {} = pure (Right x)
splitFragment f types (Spread dirs ref) = do
  _ <- validateDirectives LOCATION_FRAGMENT_SPREAD dirs
  Left <$> validateSpread f (typeName <$> types) ref
splitFragment f types (InlineFragment fragment@Fragment {..}) = do
  _ <- validateDirectives LOCATION_INLINE_FRAGMENT fragmentDirectives
  Left
    . UnionTag fragmentType
    <$> (castFragmentType Nothing fragmentPosition (typeName <$> types) fragment >>= f)

exploreFragments ::
  (ValidateFragmentSelection s) =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  OrdMap TypeName (TypeDefinition IMPLEMENTABLE VALID) ->
  SelectionSet RAW ->
  FragmentValidator s ([UnionTag], Maybe (SelectionSet RAW))
exploreFragments validateFragment types selectionSet = do
  (tags, selections) <- partitionEithers <$> traverse (splitFragment validateFragment (toList types)) (toList selectionSet)
  (tags,)
    <$> if null selections
      then pure Nothing
      else Just <$> fromElems selections

-- sorts Fragment by conditional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments ::
  [UnionTag] ->
  OrdMap TypeName (TypeDefinition IMPLEMENTABLE VALID) ->
  OrdMap TypeName [SelectionSet VALID]
tagUnionFragments fragments types = fmap categorizeType getSelectedTypes
  where
    getSelectedTypes :: OrdMap TypeName [TypeName]
    getSelectedTypes = unsafeFromList (map select fragments)
      where
        select UnionTag {unionTagName} =
          ( unionTagName,
            selectOr
              [unionTagName]
              getCompatibleTypes
              unionTagName
              types
          )
    categorizeType ::
      [TypeName] -> [SelectionSet VALID]
    categorizeType compatibleTypes =
      unionTagSelection
        <$> filter
          ((`elem` compatibleTypes) . unionTagName)
          fragments

getCompatibleTypes :: TypeDefinition a s -> [TypeName]
getCompatibleTypes TypeDefinition {typeName, typeContent = DataObject {objectImplements}} = typeName : objectImplements
getCompatibleTypes TypeDefinition {typeName} = [typeName]

maybeMerge :: [SelectionSet VALID] -> FragmentValidator s (Maybe (SelectionSet VALID))
maybeMerge [] = pure Nothing
maybeMerge (x : xs) = Just <$> startHistory (mergeConcat (x :| xs))

noEmptySelection :: FragmentValidator s a
noEmptySelection = throwError "empty selection sets are not supported."

joinClusters ::
  Maybe (SelectionSet VALID) ->
  OrdMap TypeName [SelectionSet VALID] ->
  FragmentValidator s (SelectionContent VALID)
joinClusters maybeSelSet typedSelections
  | null typedSelections = maybe noEmptySelection (pure . SelectionSet) maybeSelSet
  | otherwise =
      traverse mkUnionTag (toAssoc typedSelections)
        >>= fmap (UnionSelection maybeSelSet)
        . startHistory
        . fromElems
  where
    mkUnionTag :: (TypeName, [SelectionSet VALID]) -> FragmentValidator s UnionTag
    mkUnionTag (typeName, fragments) = UnionTag typeName <$> (maybeMerge (toList maybeSelSet <> fragments) >>= maybe noEmptySelection pure)

validateInterfaceSelection ::
  (ValidateFragmentSelection s) =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition IMPLEMENTABLE VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  TypeDefinition IMPLEMENTABLE VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateInterfaceSelection
  validateFragment
  validate
  typeDef@TypeDefinition {typeName}
  inputSelectionSet = do
    possibleTypes <- askInterfaceTypes typeDef
    (spreads, selectionSet) <- exploreFragments validateFragment possibleTypes inputSelectionSet
    validSelectionSet <- traverse (validate typeDef) selectionSet
    let tags = tagUnionFragments spreads possibleTypes
    defaultSelection <- maybeMerge (toList validSelectionSet <> selectOr [] id typeName tags)
    joinClusters defaultSelection (ordMapDelete typeName tags)

mkUnionRootType :: FragmentValidator s (TypeDefinition IMPLEMENTABLE VALID)
mkUnionRootType = (`mkType` DataObject [] empty) <$> asksScope currentTypeName

validateUnionSelection ::
  (ValidateFragmentSelection s) =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition IMPLEMENTABLE VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  UnionTypeDefinition OUT VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateUnionSelection validateFragment validate members inputSelectionSet = do
  typeDef <- mkUnionRootType
  possibleTypes <- traverse (fmap toCategory . askTypeMember) members
  (spreads, selectionSet) <- exploreFragments validateFragment possibleTypes inputSelectionSet
  validSelectionSet <- traverse (validate typeDef) selectionSet
  let tags = tagUnionFragments spreads possibleTypes
  joinClusters validSelectionSet tags
