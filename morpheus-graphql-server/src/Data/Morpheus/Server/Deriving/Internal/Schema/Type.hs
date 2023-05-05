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
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive
  ( UseDeriving (..),
    getEnumDirectives,
    getTypeDirectives,
    serializeDirectives,
    visitEnumName,
    visitEnumValueDescription,
    visitTypeDescription,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Object
  ( buildObjectTypeContent,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Union (buildUnionType)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( mapCat,
    mkEnum,
    mkScalar,
  )
import Data.Morpheus.Server.Deriving.Utils.Types
  ( CatType (..),
    GQLTypeNode (..),
    GQLTypeNodeExtension,
    withObject,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseGQLType (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    DataEnumValue (..),
    OUT,
    ScalarDefinition,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    UnionMember (..),
  )
import GHC.Generics (Rep)
import Relude

type DERIVE_TYPE gql a =
  ( gql a,
    GRep gql gql (GQLResult (ArgumentsDefinition CONST)) (Rep a)
  )

toEnumValue :: (gql a) => UseDeriving gql args -> f a -> TypeName -> GQLResult (DataEnumValue CONST)
toEnumValue ctx proxy enumName = do
  enumDirectives <- serializeDirectives ctx (getEnumDirectives ctx proxy enumName)
  pure
    DataEnumValue
      { enumName = visitEnumName ctx proxy enumName,
        enumDescription = visitEnumValueDescription ctx proxy enumName Nothing,
        ..
      }

toTypeContent ::
  (gql a) =>
  UseDeriving gql args ->
  CatType kind a ->
  GRepType (ArgumentsDefinition CONST) ->
  GQLResult (TypeContent TRUE kind CONST, [GQLTypeNodeExtension])
toTypeContent ctx prx (GRepTypeEnum variants) = (,[]) . mkEnum prx <$> traverse (toEnumValue ctx prx) variants
toTypeContent ctx prx (GRepTypeObject fields) = (,[]) <$> buildObjectTypeContent ctx prx fields
toTypeContent _ prx GRepTypeUnion {..} = buildUnionType prx (map fst variantRefs) inlineVariants

type TypeProxy gql args kind a = (UseDeriving gql args, CatType kind a)

deriveTypeContent :: (DERIVE_TYPE gql a) => TypeProxy gql args kind a -> GQLResult (TypeContent TRUE kind CONST, [GQLTypeNodeExtension])
deriveTypeContent (cxt, prx) = deriveType (fieldGRep prx cxt) prx >>= toTypeContent cxt prx

deriveTypeGuardUnions :: (DERIVE_TYPE gql a) => TypeProxy gql args OUT a -> GQLResult [TypeName]
deriveTypeGuardUnions prx = deriveTypeContent prx >>= getUnionNames prx . fst

getUnionNames :: (DERIVE_TYPE gql a) => TypeProxy gql args kind a -> TypeContent TRUE OUT CONST -> GQLResult [TypeName]
getUnionNames _ DataUnion {unionMembers} = pure $ toList $ memberName <$> unionMembers
getUnionNames (ctx, prx) DataObject {} = pure [useTypename ctx prx]
getUnionNames _ _ = throwError "guarded type must be an union or object"

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
deriveTypeDefinition ctx proxy = do
  (content, ext) <- deriveTypeContent (ctx, proxy)
  t <- fillTypeContent ctx proxy content
  pure (t, ext)

deriveInterfaceDefinition ::
  (DERIVE_TYPE gql a) =>
  UseDeriving gql args ->
  CatType OUT a ->
  GQLResult (TypeDefinition OUT CONST, [GQLTypeNodeExtension])
deriveInterfaceDefinition ctx proxy = do
  (content, ext) <- deriveTypeContent (ctx, proxy)
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
