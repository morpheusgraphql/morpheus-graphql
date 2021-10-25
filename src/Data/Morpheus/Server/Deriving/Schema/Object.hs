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
import Data.Morpheus.Server.Deriving.Schema.Enum (defineEnumUnit)
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( lookupDescription,
    lookupDirectives,
    lookupFieldContent,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    FieldRep (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CategoryValue (..),
    KindedType (..),
    outputType,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TypeData (..),
    __typeData,
  )
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
    TypeCategory,
    TypeContent (..),
    TypeDefinition,
    mkField,
    mkType,
    mkTypeRef,
    msg,
    unitFieldName,
    unitTypeName,
    unpackName,
    unsafeFromFields,
  )
import Relude hiding (empty)

defineObjectType ::
  KindedType kind a ->
  ConsRep (Maybe (FieldContent TRUE kind CONST)) ->
  SchemaT cat ()
defineObjectType proxy ConsRep {consName, consFields} = insertType . mkType consName . mkObjectTypeContent proxy =<< fields
  where
    fields
      | null consFields = defineEnumUnit $> singleton mkFieldUnit
      | otherwise = pure $ unsafeFromFields $ map repToFieldDefinition consFields

mkFieldUnit :: FieldDefinition cat s
mkFieldUnit = mkField Nothing unitFieldName (mkTypeRef unitTypeName)

buildObjectTypeContent ::
  (Applicative f, GQLType a) =>
  KindedType cat a ->
  [FieldRep (Maybe (FieldContent TRUE cat CONST))] ->
  f (TypeContent TRUE cat CONST)
buildObjectTypeContent scope consFields =
  pure
    $ mkObjectTypeContent scope
    $ unsafeFromFields
    $ map (setGQLTypeProps scope . repToFieldDefinition) consFields

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
  (GQLType a) =>
  (f a -> SchemaT kind (FieldsDefinition OUT CONST)) ->
  f a ->
  SchemaT kind (TypeDefinition OBJECT CONST)
asObjectType f proxy =
  mkType
    (gqlTypeName (__typeData (outputType proxy)))
    . DataObject []
    <$> f proxy

withObject :: (GQLType a, CategoryValue c) => KindedType c a -> TypeContent TRUE any s -> SchemaT c (FieldsDefinition c s)
withObject InputType DataInputObject {inputObjectFields} = pure inputObjectFields
withObject OutputType DataObject {objectFields} = pure objectFields
withObject x _ = failureOnlyObject x

failureOnlyObject :: forall (c :: TypeCategory) a b. (GQLType a, CategoryValue c) => KindedType c a -> SchemaT c b
failureOnlyObject proxy =
  throwError $
    msg (gqlTypeName $ __typeData proxy) <> " should have only one nonempty constructor"

mkObjectTypeContent :: KindedType kind a -> FieldsDefinition kind CONST -> TypeContent TRUE kind CONST
mkObjectTypeContent InputType = DataInputObject
mkObjectTypeContent OutputType = DataObject []

setGQLTypeProps :: GQLType a => KindedType kind a -> FieldDefinition kind CONST -> FieldDefinition kind CONST
setGQLTypeProps proxy FieldDefinition {..} =
  FieldDefinition
    { fieldName,
      fieldDescription = lookupDescription proxy key,
      fieldDirectives = lookupDirectives proxy key,
      fieldContent = lookupFieldContent proxy key <|> fieldContent,
      ..
    }
  where
    key = unpackName fieldName
