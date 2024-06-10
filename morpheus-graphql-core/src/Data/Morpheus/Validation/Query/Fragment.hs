{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Query.Fragment
  ( validateFragments,
    castFragmentType,
    validateFragment,
    selectFragmentType,
    ValidateFragmentSelection,
    validateSpread,
  )
where

import Control.Monad.Except (throwError)
import Data.Morpheus.Error.Fragment
  ( cannotBeSpreadOnType,
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation (..),
    Fragment (..),
    FragmentName,
    Fragments,
    IMPLEMENTABLE,
    Position,
    RAW,
    Ref (..),
    Selection (..),
    SelectionSet,
    Stage,
    TypeDefinition,
    TypeName,
    UnionTag (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.Validation
  ( Constraint (..),
    FragmentValidator,
    askFragments,
    askTypeDefinitions,
    constraint,
    selectKnown,
  )
import Data.Morpheus.Validation.Internal.Directive (validateDirectives)
import Relude hiding (empty)

class ValidateFragmentSelection (s :: Stage) where
  validateFragmentSelection ::
    (Applicative m) =>
    (Fragment RAW -> m (SelectionSet VALID)) ->
    Fragment s ->
    m (SelectionSet VALID)

instance ValidateFragmentSelection VALID where
  validateFragmentSelection _ = pure . fragmentSelection

instance ValidateFragmentSelection RAW where
  validateFragmentSelection f = f

validateSpread ::
  (ValidateFragmentSelection s) =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  [TypeName] ->
  Ref FragmentName ->
  FragmentValidator s UnionTag
validateSpread f allowedTargets ref = do
  fragment@Fragment {fragmentType, fragmentName} <- resolveSpread allowedTargets ref
  UnionTag fragmentType . fmap (\s -> s {selectionOrigin = Just fragmentName}) <$> validateFragmentSelection f fragment

validateFragment ::
  DirectiveLocation ->
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  [TypeName] ->
  Fragment RAW ->
  FragmentValidator s (Fragment VALID)
validateFragment loc validate allowedTypes fragment@Fragment {fragmentPosition} =
  castFragmentType Nothing fragmentPosition allowedTypes fragment
    >>= onlyValidateFrag loc validate

validateFragments ::
  (Fragment RAW -> FragmentValidator RAW (SelectionSet VALID)) ->
  FragmentValidator RAW (Fragments VALID)
validateFragments f = askFragments >>= traverse (onlyValidateFrag LOCATION_FRAGMENT_DEFINITION f)

onlyValidateFrag ::
  DirectiveLocation ->
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  Fragment RAW ->
  FragmentValidator s (Fragment VALID)
onlyValidateFrag loc validate f@Fragment {..} =
  Fragment
    fragmentName
    fragmentType
    fragmentPosition
    <$> validate f
    <*> validateDirectives loc fragmentDirectives

castFragmentType ::
  Maybe FragmentName ->
  Position ->
  [TypeName] ->
  Fragment s ->
  FragmentValidator s1 (Fragment s)
castFragmentType key position typeMembers fragment@Fragment {fragmentType}
  | fragmentType `elem` typeMembers = pure fragment
  | otherwise = throwError $ cannotBeSpreadOnType key fragmentType position typeMembers

resolveSpread :: [TypeName] -> Ref FragmentName -> FragmentValidator s (Fragment s)
resolveSpread allowedTargets ref@Ref {refName, refPosition} =
  askFragments
    >>= selectKnown ref
    >>= castFragmentType (Just refName) refPosition allowedTargets

selectFragmentType :: Fragment RAW -> FragmentValidator s (TypeDefinition IMPLEMENTABLE VALID)
selectFragmentType fr@Fragment {fragmentType, fragmentPosition} = do
  typeDef <- askTypeDefinitions >>= selectKnown (Ref fragmentType fragmentPosition)
  constraint IMPLEMENTABLE fr typeDef
