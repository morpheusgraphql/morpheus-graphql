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
import Data.Morpheus.Internal.Utils (empty)
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
  [NodeTypeVariant consName (toAny (object proxy fields))] <> [NodeUnitType | null consFields]
  where
    fields
      | null consFields = [mkField Nothing unitFieldName (mkTypeRef unitTypeName)]
      | otherwise = map (repToFieldDefinition proxy) consFields

buildObjectTypeContent ::
  (gql a) =>
  UseDeriving gql args ->
  CatType cat a ->
  [GRepField (ArgumentsDefinition CONST)] ->
  GQLResult (TypeContent TRUE cat CONST)
buildObjectTypeContent ctx proxy = fmap (object proxy) . traverse (visitFieldDefinition ctx proxy . repToFieldDefinition proxy)

object :: CatType kind a -> [FieldDefinition kind CONST] -> TypeContent TRUE kind CONST
object InputType = DataInputObject . unsafeFromFields
object OutputType = DataObject [] . unsafeFromFields

toFieldContent :: CatType c a -> ArgumentsDefinition CONST -> Maybe (FieldContent TRUE c CONST)
toFieldContent OutputType x | not (null x) = Just (FieldArgs x)
toFieldContent _ _ = Nothing

repToFieldDefinition ::
  CatType c a ->
  GRepField (ArgumentsDefinition CONST) ->
  FieldDefinition c CONST
repToFieldDefinition proxy GRepField {..} =
  FieldDefinition
    { fieldDescription = mempty,
      fieldDirectives = empty,
      fieldContent = toFieldContent proxy fieldValue,
      fieldName = fieldSelector,
      fieldType = fieldTypeRef
    }

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
