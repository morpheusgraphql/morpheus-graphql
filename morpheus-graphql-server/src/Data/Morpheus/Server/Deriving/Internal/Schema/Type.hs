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
    GRepContext (..),
    GRepType (..),
    deriveType,
    scanTypes,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive
  ( UseDeriving (..),
    deriveTypeDirectives,
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
  ( CatContext,
    addContext,
    getCatContext,
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
exploreTypes cxt proxy = scanTypes (scanCTX (getCatContext proxy) cxt) proxy

scanCTX :: (UseGQLType ctx gql) => CatContext cat -> ctx -> GRepContext gql gql Proxy (UseRef gql)
scanCTX ctx gql =
  GRepContext
    { optFun = UseRef . addContext ctx,
      optTypename = useTypename gql . addContext ctx,
      optWrappers = useWrappers gql . addContext ctx
    }

deriveTypeContentWith ::
  (DERIVE_TYPE gql a) =>
  UseDeriving gql args ->
  CatType kind a ->
  GQLResult (TypeContent TRUE kind CONST, [GQLTypeNodeExtension])
deriveTypeContentWith cxt proxy = do
  reps <- deriveType (toFieldContent (getCatContext proxy) cxt) proxy
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
  dirs <- deriveTypeDirectives ctx proxy
  pure $
    TypeDefinition
      (visitTypeDescription ctx proxy Nothing)
      (useTypename ctx proxy)
      dirs
      content

toFieldContent :: (UseGQLType ctx gql) => CatContext cat -> ctx -> GRepContext gql gql Proxy (GQLResult (ArgumentsDefinition CONST))
toFieldContent ctx gql =
  GRepContext
    { optTypename = useTypename gql . addContext ctx,
      optWrappers = useWrappers gql . addContext ctx,
      optFun = useDeriveFieldArgs gql . addContext ctx
    }

useDeriveRoot :: (UseGQLType ctx gql, gql a) => ctx -> f a -> GQLResult (TypeDefinition OBJECT CONST)
useDeriveRoot gql pr = do
  fields <- useDeriveNode gql proxy >>= nodeToType >>= withObject (useTypename gql proxy) . typeContent
  pure $ mkType (useTypename gql (outputType proxy)) (DataObject [] fields)
  where
    proxy = outputType pr
