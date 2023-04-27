{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Server.Deriving.Internal.Schema.Object
  ( buildObjectTypeContent,
    defineObjectType,
  )
where

import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils
  ( empty,
    singleton,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive
  ( UseDeriving,
    deriveFieldDirectives,
    visitFieldContent,
    visitFieldDescription,
    visitFieldName,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Enum
  (
  )
import Data.Morpheus.Server.Deriving.Utils.GRep
  ( GRepCons (..),
    GRepField (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( NodeTypeVariant (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    FieldContent (..),
    FieldDefinition (..),
    FieldsDefinition,
    TRUE,
    TypeContent (..),
    mkField,
    mkTypeRef,
    toAny,
    unitFieldName,
    unitTypeName,
    unsafeFromFields,
  )

defineObjectType :: CatType kind a -> GRepCons (Maybe (ArgumentsDefinition CONST)) -> [NodeTypeVariant]
defineObjectType proxy GRepCons {consName, consFields} =
  [NodeTypeVariant consName (toAny (mkObjectTypeContent proxy fields))] <> [NodeUnitType | null consFields]
  where
    fields
      | null consFields = singleton unitFieldName mkFieldUnit
      | otherwise = unsafeFromFields $ map (repToFieldDefinition proxy) consFields

mkFieldUnit :: FieldDefinition cat s
mkFieldUnit = mkField Nothing unitFieldName (mkTypeRef unitTypeName)

buildObjectTypeContent ::
  gql a =>
  UseDeriving gql args ->
  CatType cat a ->
  [GRepField (Maybe (ArgumentsDefinition CONST))] ->
  GQLResult (TypeContent TRUE cat CONST)
buildObjectTypeContent options scope consFields = do
  xs <- traverse (setGQLTypeProps options scope . repToFieldDefinition scope) consFields
  pure $ mkObjectTypeContent scope $ unsafeFromFields xs

mkObjectTypeContent :: CatType kind a -> FieldsDefinition kind CONST -> TypeContent TRUE kind CONST
mkObjectTypeContent InputType = DataInputObject
mkObjectTypeContent OutputType = DataObject []

repToFieldDefinition ::
  CatType c a ->
  GRepField (Maybe (ArgumentsDefinition CONST)) ->
  FieldDefinition c CONST
repToFieldDefinition
  x
  GRepField
    { fieldSelector = fieldName,
      fieldTypeRef = fieldType,
      fieldValue
    } =
    FieldDefinition
      { fieldDescription = mempty,
        fieldDirectives = empty,
        fieldContent = toFieldContent x fieldValue,
        ..
      }

toFieldContent :: CatType c a -> Maybe (ArgumentsDefinition CONST) -> Maybe (FieldContent TRUE c CONST)
toFieldContent OutputType (Just x) = Just (FieldArgs x)
toFieldContent _ _ = Nothing

setGQLTypeProps :: gql a => UseDeriving gql args -> CatType kind a -> FieldDefinition kind CONST -> GQLResult (FieldDefinition kind CONST)
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
