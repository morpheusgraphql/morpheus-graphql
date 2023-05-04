{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Morpheus.Server.Deriving.Internal.Schema.Type
  ( fillTypeContent,
    deriveTypeDefinition,
    deriveScalarDefinition,
    deriveInterfaceDefinition,
    deriveTypeGuardUnions,
    useDeriveRoot,
    exploreTypes,
    DERIVE_TYPE,
  )
where

import Control.Monad.Except
  ( MonadError (..),
  )
import Data.Foldable
import Data.Morpheus.Generic
  ( GRep,
    GRepFun (..),
    GRepType (..),
    deriveType,
    scanTypes,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive
  ( UseDeriving (..),
    getTypeDirectives,
    serializeDirectives,
    visitTypeDescription,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Enum
  ( buildEnumTypeContent,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Object
  ( buildObjectTypeContent,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Union (buildUnionType)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( mapCat,
    mkScalar,
    outputType,
  )
import Data.Morpheus.Server.Deriving.Utils.Types
  ( CatType,
    GQLTypeNode (..),
    GQLTypeNodeExtension,
    nodeToType,
    withObject,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseGQLType (..),
    UseRef (UseRef),
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    OBJECT,
    OUT,
    ScalarDefinition,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    UnionMember (..),
    mkType,
  )
import GHC.Generics (Rep)
import Relude

type DERIVE_TYPE gql a =
  ( gql a,
    GRep gql gql (GQLResult (ArgumentsDefinition CONST)) (Rep a)
  )

buildTypeContent ::
  (gql a) =>
  UseDeriving gql args ->
  CatType kind a ->
  GRepType (ArgumentsDefinition CONST) ->
  GQLResult (TypeContent TRUE kind CONST, [GQLTypeNodeExtension])
buildTypeContent options scope (GRepTypeEnum variants) = (,[]) <$> buildEnumTypeContent options scope variants
buildTypeContent options scope (GRepTypeObject fields) = (,[]) <$> buildObjectTypeContent options scope fields
buildTypeContent _ scope GRepTypeUnion {..} = buildUnionType scope (map fst variantRefs) inlineVariants

exploreTypes ::
  (gql a, GRep gql gql (UseRef gql) (Rep a)) =>
  UseDeriving gql args ->
  CatType kind a ->
  [UseRef gql]
exploreTypes cxt proxy = scanTypes (scanCTX proxy cxt) proxy

scanCTX :: (UseGQLType ctx gql) => CatType cat a -> ctx -> GRepFun gql gql Proxy (UseRef gql)
scanCTX cat gql =
  GRepFun
    { grepFun = UseRef . (`mapCat` cat),
      grepTypename = useTypename gql . (`mapCat` cat),
      grepWrappers = useWrappers gql . (`mapCat` cat)
    }

deriveTypeContentWith ::
  (DERIVE_TYPE gql a) =>
  UseDeriving gql args ->
  CatType kind a ->
  GQLResult (TypeContent TRUE kind CONST, [GQLTypeNodeExtension])
deriveTypeContentWith cxt proxy = do
  reps <- deriveType (fieldGRep proxy cxt) proxy
  buildTypeContent cxt proxy reps

deriveTypeGuardUnions ::
  (DERIVE_TYPE gql a) =>
  UseDeriving gql args ->
  CatType OUT a ->
  GQLResult [TypeName]
deriveTypeGuardUnions ctx proxy = do
  (content, _) <- deriveTypeContentWith ctx proxy
  getUnionNames content
  where
    getUnionNames :: TypeContent TRUE OUT CONST -> GQLResult [TypeName]
    getUnionNames DataUnion {unionMembers} = pure $ toList $ memberName <$> unionMembers
    getUnionNames DataObject {} = pure [useTypename ctx proxy]
    getUnionNames _ = throwError "guarded type must be an union or object"

deriveScalarDefinition ::
  (gql a) =>
  (CatType cat a -> ScalarDefinition) ->
  UseDeriving gql args ->
  CatType cat a ->
  GQLResult (GQLTypeNode cat)
deriveScalarDefinition f dir p = (`GQLTypeNode` []) <$> fillTypeContent dir p (mkScalar p (f p))

deriveTypeDefinition ::
  (DERIVE_TYPE gql a) =>
  UseDeriving gql args ->
  CatType c a ->
  GQLResult (TypeDefinition c CONST, [GQLTypeNodeExtension])
deriveTypeDefinition dir proxy = do
  (content, ext) <- deriveTypeContentWith dir proxy
  t <- fillTypeContent dir proxy content
  pure (t, ext)

deriveInterfaceDefinition ::
  (DERIVE_TYPE gql a) =>
  UseDeriving gql args ->
  CatType OUT a ->
  GQLResult (TypeDefinition OUT CONST, [GQLTypeNodeExtension])
deriveInterfaceDefinition ctx proxy = do
  (content, ext) <- deriveTypeContentWith ctx proxy
  fields <- withObject (useTypename ctx proxy) content
  t <- fillTypeContent ctx proxy (DataInterface fields)
  pure (t, ext)

fillTypeContent ::
  (gql a) =>
  UseDeriving gql args ->
  CatType c a ->
  TypeContent TRUE cat CONST ->
  GQLResult (TypeDefinition cat CONST)
fillTypeContent ctx proxy content = do
  dirs <- serializeDirectives ctx (getTypeDirectives ctx proxy)
  pure $
    TypeDefinition
      (visitTypeDescription ctx proxy Nothing)
      (useTypename ctx proxy)
      dirs
      content

fieldGRep :: (UseGQLType ctx gql) => CatType cat a -> ctx -> GRepFun gql gql Proxy (GQLResult (ArgumentsDefinition CONST))
fieldGRep cat gql =
  GRepFun
    { grepTypename = useTypename gql . (`mapCat` cat),
      grepWrappers = useWrappers gql . (`mapCat` cat),
      grepFun = useDeriveFieldArgs gql . (`mapCat` cat)
    }

useDeriveRoot :: (UseGQLType ctx gql, gql a) => ctx -> f a -> GQLResult (TypeDefinition OBJECT CONST)
useDeriveRoot gql pr = do
  fields <- useDeriveNode gql proxy >>= nodeToType >>= withObject (useTypename gql proxy) . typeContent
  pure $ mkType (useTypename gql (outputType proxy)) (DataObject [] fields)
  where
    proxy = outputType pr
