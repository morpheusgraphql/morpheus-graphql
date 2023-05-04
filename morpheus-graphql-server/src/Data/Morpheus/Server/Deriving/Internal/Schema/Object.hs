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

import Data.Morpheus.Generic
  ( GRepCons (..),
    GRepField (..),
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils
  ( empty,
    singleton,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive
  ( UseDeriving,
    getFieldDirectives,
    serializeDirectives,
    visitFieldContent,
    visitFieldDescription,
    visitFieldName,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Enum
  (
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Types (NodeTypeVariant (..))
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

defineObjectType :: CatType kind a -> GRepCons (ArgumentsDefinition CONST) -> [NodeTypeVariant]
defineObjectType proxy GRepCons {consName, consFields} =
  [NodeTypeVariant consName (toAny (mkObjectTypeContent proxy fields))] <> [NodeUnitType | null consFields]
  where
    fields
      | null consFields = singleton unitFieldName mkFieldUnit
      | otherwise = unsafeFromFields $ map (repToFieldDefinition proxy) consFields

mkFieldUnit :: FieldDefinition cat s
mkFieldUnit = mkField Nothing unitFieldName (mkTypeRef unitTypeName)

buildObjectTypeContent ::
  (gql a) =>
  UseDeriving gql args ->
  CatType cat a ->
  [GRepField (ArgumentsDefinition CONST)] ->
  GQLResult (TypeContent TRUE cat CONST)
buildObjectTypeContent ctx scope consFields = do
  mkObjectTypeContent scope . unsafeFromFields <$> traverse (visitFieldDefinition ctx scope . repToFieldDefinition scope) consFields

mkObjectTypeContent :: CatType kind a -> FieldsDefinition kind CONST -> TypeContent TRUE kind CONST
mkObjectTypeContent InputType = DataInputObject
mkObjectTypeContent OutputType = DataObject []

repToFieldDefinition ::
  CatType c a ->
  GRepField (ArgumentsDefinition CONST) ->
  FieldDefinition c CONST
repToFieldDefinition
  proxy
  GRepField
    { fieldSelector = fieldName,
      fieldTypeRef = fieldType,
      fieldValue
    } =
    FieldDefinition
      { fieldDescription = mempty,
        fieldDirectives = empty,
        fieldContent = toFieldContent proxy fieldValue,
        ..
      }

toFieldContent :: CatType c a -> ArgumentsDefinition CONST -> Maybe (FieldContent TRUE c CONST)
toFieldContent OutputType x | not (null x) = Just (FieldArgs x)
toFieldContent _ _ = Nothing

visitFieldDefinition :: (gql a) => UseDeriving gql args -> CatType kind a -> FieldDefinition kind CONST -> GQLResult (FieldDefinition kind CONST)
visitFieldDefinition ctx proxy FieldDefinition {..} = do
  dirs <- serializeDirectives ctx (getFieldDirectives ctx proxy fieldName)
  pure
    FieldDefinition
      { fieldName = visitFieldName ctx proxy fieldName,
        fieldDescription = visitFieldDescription ctx proxy fieldName Nothing,
        fieldContent = visitFieldContent ctx proxy fieldName fieldContent,
        fieldDirectives = dirs,
        ..
      }
