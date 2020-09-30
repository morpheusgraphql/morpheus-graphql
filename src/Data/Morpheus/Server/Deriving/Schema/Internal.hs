{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema.Internal
  ( KindedProxy (..),
    KindedType (..),
    builder,
    inputType,
    outputType,
    setProxyType,
    unpackMs,
    UpdateDef (..),
    withObject,
    TyContentM,
    asObjectType,
    fromSchema,
    updateByContent,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative (..))
import Control.Monad.Fail (fail)
import Data.Foldable (concatMap, traverse_)
import Data.Functor (($>), (<$>), Functor (..))
import Data.List (partition)
import qualified Data.Map as M
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    singleton,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    FieldRep (..),
    ResRep (..),
    fieldTypeName,
    isEmptyConstraint,
    isUnionRef,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TypeData (..),
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    insertType,
    updateSchema,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DataEnumValue (..),
    Description,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldName (..),
    FieldsDefinition,
    IN,
    LEAF,
    OBJECT,
    OUT,
    Schema (..),
    TRUE,
    Token,
    TypeCategory,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    UnionMember (..),
    VALID,
    mkEnumContent,
    mkField,
    mkInputValue,
    mkType,
    mkUnionMember,
    msg,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Language.Haskell.TH (Exp, Q)
import Prelude
  ( ($),
    (.),
    Bool (..),
    Show (..),
    map,
    null,
    otherwise,
    sequence,
  )

-- | context , like Proxy with multiple parameters
-- * 'kind': object, scalar, enum ...
-- * 'a': actual gql type
data KindedProxy k a
  = KindedProxy

data KindedType (cat :: TypeCategory) a where
  InputType :: KindedType IN a
  OutputType :: KindedType OUT a

-- converts:
--   f a -> KindedType IN a
-- or
--  f k a -> KindedType IN a
inputType :: f a -> KindedType IN a
inputType _ = InputType

outputType :: f a -> KindedType OUT a
outputType _ = OutputType

deriving instance Show (KindedType cat a)

setProxyType :: f b -> kinded k a -> KindedProxy k b
setProxyType _ _ = KindedProxy

fromSchema :: Eventless (Schema VALID) -> Q Exp
fromSchema Success {} = [|()|]
fromSchema Failure {errors} = fail (show errors)

withObject :: (GQLType a) => KindedType c a -> TypeContent TRUE any s -> SchemaT (FieldsDefinition c s)
withObject InputType DataInputObject {inputObjectFields} = pure inputObjectFields
withObject OutputType DataObject {objectFields} = pure objectFields
withObject x _ = failureOnlyObject x

asObjectType ::
  GQLType a =>
  (f2 a -> SchemaT (FieldsDefinition OUT CONST)) ->
  f2 a ->
  SchemaT (TypeDefinition OBJECT CONST)
asObjectType f proxy = (`mkObjectType` gqlTypeName (__type proxy)) <$> f proxy

mkObjectType :: FieldsDefinition OUT CONST -> TypeName -> TypeDefinition OBJECT CONST
mkObjectType fields typeName = mkType typeName (DataObject [] fields)

failureOnlyObject :: forall c a b. (GQLType a) => KindedType c a -> SchemaT b
failureOnlyObject proxy =
  failure
    $ globalErrorMessage
    $ msg (gqlTypeName $ __type proxy) <> " should have only one nonempty constructor"

type TyContentM kind = (SchemaT (Maybe (FieldContent TRUE kind CONST)))

type TyContent kind = Maybe (FieldContent TRUE kind CONST)

unpackM :: FieldRep (TyContentM k) -> SchemaT (FieldRep (TyContent k))
unpackM FieldRep {..} =
  FieldRep fieldSelector fieldTypeRef fieldIsObject
    <$> fieldValue

unpackCons :: ConsRep (TyContentM k) -> SchemaT (ConsRep (TyContent k))
unpackCons ConsRep {..} = ConsRep consName <$> traverse unpackM consFields

unpackMs :: [ConsRep (TyContentM k)] -> SchemaT [ConsRep (TyContent k)]
unpackMs = traverse unpackCons

builder ::
  GQLType a =>
  KindedType kind a ->
  [ConsRep (TyContent kind)] ->
  SchemaT (TypeContent TRUE kind CONST)
builder scope [ConsRep {consFields}] = buildObj <$> sequence (implements scope)
  where
    buildObj interfaces = wrapFields interfaces scope (mkFieldsDefinition consFields)
builder scope cons = genericUnion cons
  where
    typeData = __type scope
    genericUnion =
      mkUnionType scope typeData
        . analyseRep (gqlTypeName typeData)

class UpdateDef value where
  updateDef :: GQLType a => f a -> value -> value

instance UpdateDef (TypeContent TRUE c CONST) where
  updateDef proxy DataObject {objectFields = fields, ..} =
    DataObject {objectFields = fmap (updateDef proxy) fields, ..}
  updateDef proxy DataInputObject {inputObjectFields = fields} =
    DataInputObject {inputObjectFields = fmap (updateDef proxy) fields, ..}
  updateDef proxy DataInterface {interfaceFields = fields} =
    DataInterface {interfaceFields = fmap (updateDef proxy) fields, ..}
  updateDef proxy (DataEnum enums) = DataEnum $ fmap (updateDef proxy) enums
  updateDef _ x = x

instance GetFieldContent cat => UpdateDef (FieldDefinition cat CONST) where
  updateDef proxy FieldDefinition {fieldName, fieldType, fieldContent} =
    FieldDefinition
      { fieldName,
        fieldDescription = lookupDescription (readName fieldName) proxy,
        fieldDirectives = lookupDirectives (readName fieldName) proxy,
        fieldContent = getFieldContent fieldName fieldContent proxy,
        ..
      }

instance UpdateDef (DataEnumValue CONST) where
  updateDef proxy DataEnumValue {enumName} =
    DataEnumValue
      { enumName,
        enumDescription = lookupDescription (readTypeName enumName) proxy,
        enumDirectives = lookupDirectives (readTypeName enumName) proxy
      }

lookupDescription :: GQLType a => Token -> f a -> Maybe Description
lookupDescription name = (name `M.lookup`) . getDescriptions

lookupDirectives :: GQLType a => Token -> f a -> Directives CONST
lookupDirectives name = fromMaybe [] . (name `M.lookup`) . getDirectives

class GetFieldContent c where
  getFieldContent :: GQLType a => FieldName -> Maybe (FieldContent TRUE c CONST) -> f a -> Maybe (FieldContent TRUE c CONST)

instance GetFieldContent IN where
  getFieldContent name val proxy =
    case name `M.lookup` getFieldContents proxy of
      Just (Just x, _) -> Just (DefaultInputValue x)
      _ -> val

instance GetFieldContent OUT where
  getFieldContent name args proxy =
    case name `M.lookup` getFieldContents proxy of
      Just (_, Just x) -> Just (FieldArgs x)
      _ -> args

updateByContent ::
  GQLType a =>
  (f kind a -> SchemaT (TypeContent TRUE cat CONST)) ->
  f kind a ->
  SchemaT ()
updateByContent f proxy =
  updateSchema
    (gqlFingerprint $ __type proxy)
    deriveD
    proxy
  where
    deriveD =
      fmap
        ( TypeDefinition
            (description proxy)
            (gqlTypeName (__type proxy))
            []
        )
        . f

analyseRep :: TypeName -> [ConsRep (Maybe (FieldContent TRUE kind CONST))] -> ResRep (Maybe (FieldContent TRUE kind CONST))
analyseRep baseName cons =
  ResRep
    { enumCons = fmap consName enumRep,
      unionRef = fieldTypeName <$> concatMap consFields unionRefRep,
      unionRecordRep
    }
  where
    (enumRep, left1) = partition isEmptyConstraint cons
    (unionRefRep, unionRecordRep) = partition (isUnionRef baseName) left1

mkUnionType ::
  KindedType kind a ->
  TypeData ->
  ResRep (Maybe (FieldContent TRUE kind CONST)) ->
  SchemaT (TypeContent TRUE kind CONST)
mkUnionType InputType _ ResRep {unionRef = [], unionRecordRep = [], enumCons} = pure $ mkEnumContent enumCons
mkUnionType OutputType _ ResRep {unionRef = [], unionRecordRep = [], enumCons} = pure $ mkEnumContent enumCons
mkUnionType InputType _ ResRep {unionRef, unionRecordRep, enumCons} = DataInputUnion <$> typeMembers
  where
    typeMembers :: SchemaT [UnionMember IN CONST]
    typeMembers = withMembers <$> buildUnions unionRecordRep
      where
        withMembers unionMembers = fmap mkUnionMember (unionRef <> unionMembers) <> fmap (`UnionMember` False) enumCons
mkUnionType OutputType typeData ResRep {unionRef, unionRecordRep, enumCons} = DataUnion . map mkUnionMember <$> typeMembers
  where
    typeMembers = do
      enums <- buildUnionEnum typeData enumCons
      unions <- buildUnions unionRecordRep
      pure (unionRef <> enums <> unions)

wrapFields :: [TypeName] -> KindedType kind a -> FieldsDefinition kind CONST -> TypeContent TRUE kind CONST
wrapFields _ InputType = DataInputObject
wrapFields interfaces OutputType = DataObject interfaces

mkFieldsDefinition :: [FieldRep (Maybe (FieldContent TRUE kind CONST))] -> FieldsDefinition kind CONST
mkFieldsDefinition = unsafeFromFields . fmap fieldByRep

fieldByRep :: FieldRep (Maybe (FieldContent TRUE kind CONST)) -> FieldDefinition kind CONST
fieldByRep FieldRep {fieldSelector, fieldTypeRef, fieldValue} =
  mkField fieldValue fieldSelector fieldTypeRef

buildUnions ::
  PackObject kind =>
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT [TypeName]
buildUnions cons =
  traverse_ buildURecType cons $> fmap consName cons
  where
    buildURecType = insertType . buildUnionRecord

buildUnionRecord ::
  PackObject kind =>
  ConsRep (Maybe (FieldContent TRUE kind CONST)) ->
  TypeDefinition kind CONST
buildUnionRecord ConsRep {consName, consFields} =
  mkType consName (packObject $ mkFieldsDefinition consFields)

class PackObject kind where
  packObject :: FieldsDefinition kind CONST -> TypeContent TRUE kind CONST

instance PackObject OUT where
  packObject = DataObject []

instance PackObject IN where
  packObject = DataInputObject

buildUnionEnum ::
  TypeData ->
  [TypeName] ->
  SchemaT [TypeName]
buildUnionEnum TypeData {gqlTypeName} enums = updates $> members
  where
    members
      | null enums = []
      | otherwise = [enumTypeWrapperName]
    enumTypeName = gqlTypeName <> "Enum"
    enumTypeWrapperName = enumTypeName <> "Object"
    -------------------------
    updates :: SchemaT ()
    updates
      | null enums = pure ()
      | otherwise =
        buildEnumObject enumTypeWrapperName enumTypeName
          *> buildEnum enumTypeName enums

buildEnum :: TypeName -> [TypeName] -> SchemaT ()
buildEnum typeName tags =
  insertType
    ( mkType typeName (mkEnumContent tags) ::
        TypeDefinition LEAF CONST
    )

buildEnumObject :: TypeName -> TypeName -> SchemaT ()
buildEnumObject typeName enumTypeName =
  insertType
    ( mkType
        typeName
        ( DataObject []
            $ singleton
            $ mkInputValue "enum" [] enumTypeName
        ) ::
        TypeDefinition OBJECT CONST
    )
