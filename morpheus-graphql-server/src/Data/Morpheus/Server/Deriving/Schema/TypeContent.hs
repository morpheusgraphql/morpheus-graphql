{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Server.Deriving.Schema.TypeContent
  ( buildTypeContent,
    insertTypeContent,
    deriveTypeContentWith,
    deriveFieldsWith,
    deriveTypeDefinition,
    insertType,
    toFieldContent,
    deriveScalarDefinition,
  )
where

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

insertTypeContent ::
  forall k gql args a.
  (gql a) =>
  UseDirective gql args ->
  (CatType k a -> SchemaT k (TypeContent TRUE k CONST)) ->
  CatType k a ->
  SchemaT k ()
insertTypeContent options@UseDirective {dirGQL = UseGQLType {..}} f proxy =
  updateSchema
    (useFingerprint proxy)
    deriveD
    proxy
  where
    deriveD x = do
      content <- f x
      dirs <- deriveTypeDirectives options proxy
      pure $
        TypeDefinition
          (visitTypeDescription options proxy Nothing)
          (useTypename proxy)
          dirs
          content

deriveTypeContentWith ::
  ( DeriveWith gql derive (SchemaT kind (TyContent kind)) (Rep a),
    gql a
  ) =>
  UseDirective gql args ->
  DeriveTypeOptions kind gql derive (SchemaT kind (TyContent kind)) ->
  CatType kind a ->
  SchemaT kind (TypeContent TRUE kind CONST)
deriveTypeContentWith options x kindedProxy =
  unpackMonad
    ( deriveTypeWith x kindedProxy
    )
    >>= buildTypeContent options kindedProxy

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
deriveTypeDefinition dir proxy =
  deriveTypeContentWith dir (toFieldContent (getCatContext proxy) dir) proxy
    >>= fillTypeContent dir proxy

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

deriveFieldsWith ::
  ( gql a,
    DeriveWith gql derive (SchemaT cat (TyContent cat)) (Rep a)
  ) =>
  UseDirective gql args ->
  DeriveTypeOptions cat gql derive (SchemaT cat (TyContent cat)) ->
  CatType cat a ->
  SchemaT cat (FieldsDefinition cat CONST)
deriveFieldsWith dirs cont kindedType = deriveTypeContentWith dirs cont kindedType >>= withObject (dirGQL dirs) kindedType

toFieldContent :: CatContext cat -> UseDirective gql dir -> DeriveTypeOptions cat gql gql (TyContentM cat)
toFieldContent ctx UseDirective {..} =
  DeriveTypeOptions
    { __typeGetType = useTypeData dirGQL . addContext ctx,
      __typeApply = \proxy -> useDeriveType dirGQL (addContext ctx proxy) *> useDeriveContent dirGQL (addContext ctx proxy)
    }
