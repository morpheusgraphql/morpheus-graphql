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
  )
where

import Data.Morpheus.Server.Deriving.Schema.Directive (UseDirective (..), deriveTypeDirectives, visitTypeDescription)
import Data.Morpheus.Server.Deriving.Schema.Enum
  ( buildEnumTypeContent,
  )
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( CatType,
    TyContent,
  )
import Data.Morpheus.Server.Deriving.Schema.Object
  ( buildObjectTypeContent,
    withObject,
  )
import Data.Morpheus.Server.Deriving.Schema.Union (buildUnionTypeContent)
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    DeriveTypeOptions,
    DeriveWith,
    deriveTypeWith,
    isEmptyConstraint,
    unpackMonad,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (typeCat)
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
    (__useFingerprint category proxy)
    deriveD
    proxy
  where
    category = typeCat proxy
    deriveD x = do
      content <- f x
      dirs <- deriveTypeDirectives options proxy
      pure $
        TypeDefinition
          (visitTypeDescription options proxy Nothing)
          (__useTypename category proxy)
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

deriveFieldsWith ::
  ( gql a,
    DeriveWith gql derive (SchemaT cat (TyContent cat)) (Rep a)
  ) =>
  UseDirective gql args ->
  DeriveTypeOptions cat gql derive (SchemaT cat (TyContent cat)) ->
  CatType cat a ->
  SchemaT cat (FieldsDefinition cat CONST)
deriveFieldsWith dirs cont kindedType = deriveTypeContentWith dirs cont kindedType >>= withObject (dirGQL dirs) kindedType
