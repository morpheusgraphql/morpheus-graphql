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
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( NodeDerivation (NodeExtension),
    SchemaBuilder (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Types (CatType, GQLTypeNodeExtension, nodeToType, withObject)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( FieldRep (..),
    UseGQLType (..),
    UseRef (UseRef),
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
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

buildTypeContent ::
  (gql a) =>
  UseDeriving gql args ->
  CatType kind a ->
  GRepType FieldRep ->
  GQLResult (TypeContent TRUE kind CONST, [GQLTypeNodeExtension])
buildTypeContent options scope (GRepTypeEnum variants) = (,[]) <$> buildEnumTypeContent options scope variants
buildTypeContent options scope (GRepTypeObject fields) = (,[]) <$> buildObjectTypeContent options scope (map (unFieldRep <$>) fields)
buildTypeContent _ scope GRepTypeUnion {..} = buildUnionType scope (map fst variantRefs) (map (unFieldRep <$>) inlineVariants)

exploreTypes ::
  (gql a, GRep gql gql (UseRef gql) (Rep a)) =>
  UseDeriving gql args ->
  CatType kind a ->
  [UseRef gql]
exploreTypes UseDeriving {..} proxy =
  scanTypes (scanCTX (getCatContext proxy) drvGQL) proxy

scanCTX :: CatContext cat -> UseGQLType gql -> GRepContext gql gql Proxy (UseRef gql)
scanCTX ctx gql =
  GRepContext
    { optTypeData = useTypeData gql . addContext ctx,
      optApply = UseRef . addContext ctx
    }

deriveTypeContentWith ::
  (gql a, GRep gql gql (SchemaBuilder FieldRep) (Rep a)) =>
  UseDeriving gql args ->
  CatType kind a ->
  SchemaBuilder (TypeContent TRUE kind CONST)
deriveTypeContentWith drv@UseDeriving {..} proxy = do
  reps <- deriveType (toFieldContent (getCatContext proxy) drvGQL) proxy
  SchemaBuilder $ do
    (t, ext) <- buildTypeContent drv proxy reps
    pure (t, map NodeExtension ext)

deriveTypeGuardUnions ::
  (gql a, GRep gql gql (SchemaBuilder FieldRep) (Rep a)) =>
  UseDeriving gql args ->
  CatType OUT a ->
  SchemaBuilder [TypeName]
deriveTypeGuardUnions drv proxy = do
  content <- deriveTypeContentWith drv proxy
  getUnionNames content
  where
    getUnionNames :: TypeContent TRUE OUT CONST -> SchemaBuilder [TypeName]
    getUnionNames DataUnion {unionMembers} = pure $ toList $ memberName <$> unionMembers
    getUnionNames DataObject {} = pure [useTypename (drvGQL drv) proxy]
    getUnionNames _ = throwError "guarded type must be an union or object"

deriveScalarDefinition ::
  gql a =>
  (CatType cat a -> ScalarDefinition) ->
  UseDeriving gql args ->
  CatType cat a ->
  SchemaBuilder (TypeDefinition cat CONST)
deriveScalarDefinition f dir p = fillTypeContent dir p (mkScalar p (f p))

deriveTypeDefinition ::
  (gql a, GRep gql gql (SchemaBuilder FieldRep) (Rep a)) =>
  UseDeriving gql args ->
  CatType c a ->
  SchemaBuilder (TypeDefinition c CONST)
deriveTypeDefinition dir proxy = do
  content <- deriveTypeContentWith dir proxy
  fillTypeContent dir proxy content

deriveInterfaceDefinition ::
  (gql a, GRep gql gql (SchemaBuilder FieldRep) (Rep a)) =>
  UseDeriving gql args ->
  CatType OUT a ->
  SchemaBuilder (TypeDefinition OUT CONST)
deriveInterfaceDefinition drv proxy = do
  content <- deriveTypeContentWith drv proxy
  fields <- withObject (useTypename (drvGQL drv) proxy) content
  fillTypeContent drv proxy (DataInterface fields)

fillTypeContent ::
  gql a =>
  UseDeriving gql args ->
  CatType c a ->
  TypeContent TRUE cat CONST ->
  SchemaBuilder (TypeDefinition cat CONST)
fillTypeContent options@UseDeriving {drvGQL = UseGQLType {..}} proxy content = do
  dirs <- SchemaBuilder ((,[]) <$> deriveTypeDirectives options proxy)
  pure $
    TypeDefinition
      (visitTypeDescription options proxy Nothing)
      (useTypename proxy)
      dirs
      content

toFieldContent :: CatContext cat -> UseGQLType gql -> GRepContext gql gql Proxy (SchemaBuilder FieldRep)
toFieldContent ctx gql =
  GRepContext
    { optTypeData = useTypeData gql . addContext ctx,
      optApply = useDeriveFieldArguments gql . addContext ctx
    }

useDeriveRoot :: gql a => UseGQLType gql -> f a -> SchemaBuilder (TypeDefinition OBJECT CONST)
useDeriveRoot gql pr = do
  fields <- useDeriveNode gql proxy >>= nodeToType >>= withObject (useTypename gql proxy) . typeContent
  pure $ mkType (useTypename gql (outputType proxy)) (DataObject [] fields)
  where
    proxy = outputType pr
