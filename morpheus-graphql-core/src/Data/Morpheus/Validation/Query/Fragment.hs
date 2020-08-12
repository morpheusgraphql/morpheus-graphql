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

import Control.Applicative ((<*>), pure)
import Control.Monad ((>>=))
import Data.Functor ((<$>))
import Data.List (elem)
import Data.Maybe (Maybe (..))
-- MORPHEUS
import Data.Morpheus.Error.Fragment
  ( cannotBeSpreadOnType,
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Directives,
    FieldName,
    Fragment (..),
    Fragments,
    OUTPUT_OBJECT,
    Position,
    RAW,
    Ref (..),
    Schema,
    SelectionSet,
    Stage,
    Stage,
    TypeDefinition,
    TypeName,
    TypeNameRef (..),
    UnionTag (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.Validation
  ( Constraint (..),
    FragmentValidator,
    askFragments,
    askSchema,
    constraint,
    selectKnown,
  )
import Data.Traversable (traverse)
import Prelude
  ( ($),
    otherwise,
  )

class ResolveFragment (s :: Stage) where
  resolveValidFragment ::
    (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
    [TypeName] ->
    Ref ->
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
validateFragmentDirectives _ = pure [] --TODO: validate fragment directives

castFragmentType ::
  Maybe FieldName ->
  Position ->
  [TypeName] ->
  Fragment s ->
  FragmentValidator s1 (Fragment s)
castFragmentType key position typeMembers fragment@Fragment {fragmentType}
  | fragmentType `elem` typeMembers = pure fragment
  | otherwise = failure $ cannotBeSpreadOnType key fragmentType position typeMembers

resolveSpread :: [TypeName] -> Ref -> FragmentValidator s (Fragment s)
resolveSpread allowedTargets ref@Ref {refName, refPosition} =
  askFragments
    >>= selectKnown ref
    >>= castFragmentType (Just refName) refPosition allowedTargets

selectFragmentType :: Fragment RAW -> FragmentValidator s (TypeDefinition OUTPUT_OBJECT VALID)
selectFragmentType fr@Fragment {fragmentType, fragmentPosition} = do
  (schema :: Schema VALID) <- askSchema
  typeDef <- selectKnown (TypeNameRef fragmentType fragmentPosition) schema
  constraint OBJECT fr typeDef
