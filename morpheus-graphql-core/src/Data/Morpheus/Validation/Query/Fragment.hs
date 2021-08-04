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
    resolveSpread,
    validateFragment,
    selectFragmentType,
    ResolveFragment (..),
  )
where

import Control.Monad.Except (throwError)
import Data.Morpheus.Error.Fragment
  ( cannotBeSpreadOnType,
  )
import Data.Morpheus.Internal.Utils
  ( Empty (empty),
  )
import Data.Morpheus.Types.Internal.AST
  ( Directives,
    Fragment (..),
    FragmentName,
    Fragments,
    IMPLEMENTABLE,
    Position,
    RAW,
    Ref (..),
    SelectionSet,
    Stage,
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
import Relude hiding (empty)

class ResolveFragment (s :: Stage) where
  resolveValidFragment ::
    (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
    [TypeName] ->
    Ref FragmentName ->
    FragmentValidator s UnionTag

instance ResolveFragment VALID where
  resolveValidFragment _ allowedTargets ref = do
    Fragment {fragmentType, fragmentSelection} <- resolveSpread allowedTargets ref
    pure $ UnionTag fragmentType fragmentSelection

instance ResolveFragment RAW where
  resolveValidFragment f allowedTargets ref = do
    fragment@Fragment {fragmentType} <- resolveSpread allowedTargets ref
    UnionTag fragmentType <$> f fragment

validateFragment ::
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  [TypeName] ->
  Fragment RAW ->
  FragmentValidator s (Fragment VALID)
validateFragment validate allowedTypes fragment@Fragment {fragmentPosition} =
  castFragmentType Nothing fragmentPosition allowedTypes fragment
    >>= onlyValidateFrag validate

validateFragments ::
  (Fragment RAW -> FragmentValidator RAW (SelectionSet VALID)) ->
  FragmentValidator RAW (Fragments VALID)
validateFragments f = askFragments >>= traverse (onlyValidateFrag f)

onlyValidateFrag ::
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  Fragment RAW ->
  FragmentValidator s (Fragment VALID)
onlyValidateFrag validate f@Fragment {..} =
  Fragment
    fragmentName
    fragmentType
    fragmentPosition
    <$> validate f <*> validateFragmentDirectives fragmentDirectives

validateFragmentDirectives :: Directives RAW -> FragmentValidator s (Directives VALID)
validateFragmentDirectives _ = pure empty --TODO: validate fragment directives

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
