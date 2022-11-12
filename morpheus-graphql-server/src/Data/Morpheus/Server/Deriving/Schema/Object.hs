{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Server.Deriving.Schema.Object
  ( asObjectType,
    withObject,
    buildObjectTypeContent,
    defineObjectType,
  )
where

import Control.Monad.Except (throwError)
import Data.Morpheus.Internal.Utils
  ( empty,
    singleton,
  )
import Data.Morpheus.Server.Deriving.Schema.Directive
  ( UseDirective,
    deriveFieldDirectives,
    visitFieldContent,
    visitFieldDescription,
    visitFieldName,
  )
import Data.Morpheus.Server.Deriving.Schema.Enum (defineEnumUnit)
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    FieldRep (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    outputType,
  )
import Data.Morpheus.Server.Deriving.Utils.Use (UseGQLType (..))
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    insertType,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    FieldContent (..),
    FieldDefinition (..),
    FieldsDefinition,
    OBJECT,
    OUT,
    TRUE,
    TypeContent (..),
    TypeDefinition,
    mkField,
    mkType,
    mkTypeRef,
    msg,
    unitFieldName,
    unitTypeName,
    unsafeFromFields,
  )
import Relude hiding (empty)

defineObjectType ::
  CatType kind a ->
  ConsRep (Maybe (FieldContent TRUE kind CONST)) ->
  SchemaT cat ()
defineObjectType proxy ConsRep {consName, consFields} = insertType . mkType consName . mkObjectTypeContent proxy =<< fields
  where
    fields
      | null consFields = defineEnumUnit $> singleton unitFieldName mkFieldUnit
      | otherwise = pure $ unsafeFromFields $ map repToFieldDefinition consFields

mkFieldUnit :: FieldDefinition cat s
mkFieldUnit = mkField Nothing unitFieldName (mkTypeRef unitTypeName)

buildObjectTypeContent ::
  gql a =>
  UseDirective gql args ->
  CatType cat a ->
  [FieldRep (Maybe (FieldContent TRUE cat CONST))] ->
  SchemaT k (TypeContent TRUE cat CONST)
buildObjectTypeContent options scope consFields = do
  xs <- traverse (setGQLTypeProps options scope . repToFieldDefinition) consFields
  pure $ mkObjectTypeContent scope $ unsafeFromFields xs

repToFieldDefinition ::
  FieldRep (Maybe (FieldContent TRUE kind CONST)) ->
  FieldDefinition kind CONST
repToFieldDefinition
  FieldRep
    { fieldSelector = fieldName,
      fieldTypeRef = fieldType,
      fieldValue
    } =
    FieldDefinition
      { fieldDescription = mempty,
        fieldDirectives = empty,
        fieldContent = fieldValue,
        ..
      }

asObjectType ::
  (gql a) =>
  UseGQLType gql ->
  (f a -> SchemaT kind (FieldsDefinition OUT CONST)) ->
  f a ->
  SchemaT kind (TypeDefinition OBJECT CONST)
asObjectType gql f proxy =
  mkType
    (__useTypename gql (outputType proxy))
    . DataObject []
    <$> f proxy

withObject :: (gql a) => UseGQLType gql -> CatType c a -> TypeContent TRUE any s -> SchemaT c (FieldsDefinition c s)
withObject _ InputType DataInputObject {inputObjectFields} = pure inputObjectFields
withObject _ OutputType DataObject {objectFields} = pure objectFields
withObject gql x _ = failureOnlyObject gql x

failureOnlyObject :: (gql a) => UseGQLType gql -> CatType c a -> SchemaT c b
failureOnlyObject gql proxy = throwError $ msg (__useTypename gql proxy) <> " should have only one nonempty constructor"

mkObjectTypeContent :: CatType kind a -> FieldsDefinition kind CONST -> TypeContent TRUE kind CONST
mkObjectTypeContent InputType = DataInputObject
mkObjectTypeContent OutputType = DataObject []

setGQLTypeProps :: gql a => UseDirective gql args -> CatType kind a -> FieldDefinition kind CONST -> SchemaT k (FieldDefinition kind CONST)
setGQLTypeProps options proxy FieldDefinition {..} = do
  dirs <- deriveFieldDirectives options proxy fieldName
  pure
    FieldDefinition
      { fieldName = visitFieldName options proxy fieldName,
        fieldDescription = visitFieldDescription options proxy fieldName Nothing,
        fieldContent = visitFieldContent options proxy fieldName fieldContent,
        fieldDirectives = dirs,
        ..
      }
