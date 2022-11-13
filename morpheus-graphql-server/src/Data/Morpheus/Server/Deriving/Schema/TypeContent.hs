{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Server.Deriving.Schema.TypeContent
  ( insertType,
    deriveFields,
    deriveTypeDefinition,
    deriveScalarDefinition,
    deriveInterfaceDefinition,
    deriveTypeGuardUnions,
  )
where

import Control.Monad.Except
import Data.Foldable
import Data.Morpheus.Server.Deriving.Schema.Directive (UseDirective (..), deriveTypeDirectives, visitTypeDescription)
import Data.Morpheus.Server.Deriving.Schema.Enum
  ( buildEnumTypeContent,
  )
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( CatType,
    TyContent,
    TyContentM,
  )
import Data.Morpheus.Server.Deriving.Schema.Object
  ( buildObjectTypeContent,
    withObject,
  )
import Data.Morpheus.Server.Deriving.Schema.Union (buildUnionTypeContent)
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    DeriveTypeOptions (..),
    DeriveWith,
    deriveTypeWith,
    isEmptyConstraint,
    unpackMonad,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (CatContext, addContext, getCatContext, mkScalar)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseGQLType (..),
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    updateSchema,
  )
import Data.Morpheus.Types.Internal.AST
import GHC.Generics (Rep)

buildTypeContent ::
  (gql a) =>
  UseDirective gql args ->
  CatType kind a ->
  [ConsRep (TyContent kind)] ->
  SchemaT kind (TypeContent TRUE kind CONST)
buildTypeContent options scope cons | all isEmptyConstraint cons = buildEnumTypeContent options scope (consName <$> cons)
buildTypeContent options scope [ConsRep {consFields}] = buildObjectTypeContent options scope consFields
buildTypeContent options scope cons = buildUnionTypeContent (dirGQL options) scope cons

deriveTypeContentWith ::
  (gql a, DeriveWith gql gql (SchemaT kind (TyContent kind)) (Rep a)) =>
  UseDirective gql args ->
  CatType kind a ->
  SchemaT kind (TypeContent TRUE kind CONST)
deriveTypeContentWith dir proxy =
  unpackMonad (deriveTypeWith (toFieldContent (getCatContext proxy) dir) proxy)
    >>= buildTypeContent dir proxy

deriveTypeGuardUnions ::
  ( gql a,
    DeriveWith gql gql (SchemaT OUT (TyContent OUT)) (Rep a)
  ) =>
  UseDirective gql args ->
  CatType OUT a ->
  SchemaT OUT [TypeName]
deriveTypeGuardUnions dir proxy = do
  content <- deriveTypeContentWith dir proxy
  getUnionNames content
  where
    getUnionNames :: TypeContent TRUE OUT CONST -> SchemaT OUT [TypeName]
    getUnionNames DataUnion {unionMembers} = pure $ toList $ memberName <$> unionMembers
    getUnionNames DataObject {} = pure [useTypename (dirGQL dir) proxy]
    getUnionNames _ = throwError "guarded type must be an union or object"

insertType ::
  forall c gql a args.
  (gql a) =>
  UseDirective gql args ->
  (UseDirective gql args -> CatType c a -> SchemaT c (TypeDefinition c CONST)) ->
  CatType c a ->
  SchemaT c ()
insertType dir f proxy = updateSchema (useFingerprint (dirGQL dir) proxy) (f dir) proxy

deriveScalarDefinition ::
  gql a =>
  (CatType cat a -> ScalarDefinition) ->
  UseDirective gql args ->
  CatType cat a ->
  SchemaT kind (TypeDefinition cat CONST)
deriveScalarDefinition f dir p = fillTypeContent dir p (mkScalar p (f p))

deriveTypeDefinition ::
  (gql a, DeriveWith gql gql (SchemaT c (TyContent c)) (Rep a)) =>
  UseDirective gql args ->
  CatType c a ->
  SchemaT c (TypeDefinition c CONST)
deriveTypeDefinition dir proxy = deriveTypeContentWith dir proxy >>= fillTypeContent dir proxy

deriveInterfaceDefinition ::
  (gql a, DeriveWith gql gql (SchemaT OUT (TyContent OUT)) (Rep a)) =>
  UseDirective gql args ->
  CatType OUT a ->
  SchemaT OUT (TypeDefinition OUT CONST)
deriveInterfaceDefinition dir proxy = do
  fields <- deriveFields dir proxy
  fillTypeContent dir proxy (DataInterface fields)

fillTypeContent ::
  gql a =>
  UseDirective gql args ->
  CatType c a ->
  TypeContent TRUE cat CONST ->
  SchemaT kind (TypeDefinition cat CONST)
fillTypeContent options@UseDirective {dirGQL = UseGQLType {..}} proxy content = do
  dirs <- deriveTypeDirectives options proxy
  pure $
    TypeDefinition
      (visitTypeDescription options proxy Nothing)
      (useTypename proxy)
      dirs
      content

deriveFields ::
  ( gql a,
    DeriveWith gql gql (SchemaT cat (TyContent cat)) (Rep a)
  ) =>
  UseDirective gql args ->
  CatType cat a ->
  SchemaT cat (FieldsDefinition cat CONST)
deriveFields dirs kindedType = deriveTypeContentWith dirs kindedType >>= withObject (dirGQL dirs) kindedType

toFieldContent :: CatContext cat -> UseDirective gql dir -> DeriveTypeOptions cat gql gql (TyContentM cat)
toFieldContent ctx UseDirective {..} =
  DeriveTypeOptions
    { __typeGetType = useTypeData dirGQL . addContext ctx,
      __typeApply = \proxy -> useDeriveType dirGQL (addContext ctx proxy) *> useDeriveContent dirGQL (addContext ctx proxy)
    }
