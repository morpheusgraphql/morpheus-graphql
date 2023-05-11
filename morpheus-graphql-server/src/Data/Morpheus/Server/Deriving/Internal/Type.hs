{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Morpheus.Server.Deriving.Internal.Type
  ( deriveTypeDefinition,
    deriveScalarDefinition,
    deriveInterfaceDefinition,
    deriveTypeGuardUnions,
    toTypeDefinition,
    DERIVE_TYPE,
  )
where

import Control.Monad.Except
  ( MonadError (..),
  )
import Data.Foldable
import Data.List (partition)
import Data.Morpheus.Generic
  ( GRep,
    GRepCons (..),
    GRepField (..),
    GRepFun (..),
    GRepType (..),
    deriveType,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils (Empty (..), fromElems)
import Data.Morpheus.Server.Deriving.Internal.Directive
  ( UseDeriving (..),
    getEnumDirectives,
    getFieldDirectives,
    getTypeDirectives,
    serializeDirectives,
    visitEnumName,
    visitEnumValueDescription,
    visitFieldContent,
    visitFieldDescription,
    visitFieldName,
    visitTypeDescription,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( mapCat,
    mkEnum,
    mkObject,
    mkScalar,
  )
import Data.Morpheus.Server.Deriving.Utils.Types
  ( CatType (..),
    GQLTypeNode (..),
    GQLTypeNodeExtension (..),
    NodeTypeVariant (..),
    mapTypeContent,
    toFieldContent,
    withObject,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseGQLType (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    DataEnumValue (..),
    FieldDefinition (..),
    Msg (msg),
    OUT,
    ScalarDefinition,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    UnionMember (..),
    mkField,
    mkNullaryMember,
    mkTypeRef,
    mkUnionMember,
    toAny,
    unitFieldName,
    unitTypeName,
  )
import GHC.Generics (Rep)
import Relude hiding (empty)

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

repToField ::
  CatType c a ->
  GRepField (ArgumentsDefinition CONST) ->
  FieldDefinition c CONST
repToField proxy GRepField {..} =
  FieldDefinition
    { fieldDescription = mempty,
      fieldDirectives = empty,
      fieldContent = toFieldContent proxy fieldValue,
      fieldName = fieldSelector,
      fieldType = fieldTypeRef
    }

visitField :: (gql a) => UseDeriving gql args -> CatType kind a -> FieldDefinition kind CONST -> GQLResult (FieldDefinition kind CONST)
visitField ctx proxy FieldDefinition {..} = do
  dirs <- serializeDirectives ctx (getFieldDirectives ctx proxy fieldName)
  pure
    FieldDefinition
      { fieldName = visitFieldName ctx proxy fieldName,
        fieldDescription = visitFieldDescription ctx proxy fieldName Nothing,
        fieldContent = visitFieldContent ctx proxy fieldName fieldContent,
        fieldDirectives = dirs,
        ..
      }

toUnion ::
  CatType kind a ->
  [TypeName] ->
  [GRepCons (ArgumentsDefinition CONST)] ->
  GQLResult (TypeContent TRUE kind CONST, [GQLTypeNodeExtension])
toUnion prx@InputType variantRefs inlineVariants = do
  let nodes = [UnionVariantsExtension ([NodeUnitType | not (null nullaryVariants)] <> concat (traverse (toTypeVariants prx) objectVariants))]
  variants <- fromElems members
  pure (DataInputUnion variants, nodes)
  where
    (nullaryVariants, objectVariants) = partition null inlineVariants
    members =
      map mkUnionMember (variantRefs <> map consName objectVariants)
        <> fmap (mkNullaryMember . consName) nullaryVariants
toUnion prx@OutputType unionRef unionCons = do
  variants <- fromElems (map mkUnionMember (unionRef <> map consName unionCons))
  pure (DataUnion variants, [UnionVariantsExtension (concat $ traverse (toTypeVariants prx) unionCons)])

toTypeVariants :: CatType kind a -> GRepCons (ArgumentsDefinition CONST) -> [NodeTypeVariant]
toTypeVariants proxy GRepCons {consName, consFields} =
  [NodeTypeVariant consName (toAny (mkObject proxy fields))] <> [NodeUnitType | null consFields]
  where
    fields
      | null consFields = [mkField Nothing unitFieldName (mkTypeRef unitTypeName)]
      | otherwise = map (repToField proxy) consFields

toTypeContent ::
  (gql a) =>
  UseDeriving gql args ->
  CatType kind a ->
  GRepType (ArgumentsDefinition CONST) ->
  GQLResult (TypeContent TRUE kind CONST, [GQLTypeNodeExtension])
toTypeContent ctx prx (GRepTypeEnum variants) = (,[]) . mkEnum prx <$> traverse (toEnumValue ctx prx) variants
toTypeContent ctx prx (GRepTypeObject fields) = (,[]) . mkObject prx <$> traverse (visitField ctx prx . repToField prx) fields
toTypeContent _ prx GRepTypeUnion {..} = toUnion prx (map fst variantRefs) inlineVariants

type TypeProxy gql args kind a = (UseDeriving gql args, CatType kind a)

toTypeDefinition ::
  (gql a) =>
  UseDeriving gql args ->
  CatType c a ->
  TypeContent TRUE cat CONST ->
  GQLResult (TypeDefinition cat CONST)
toTypeDefinition ctx proxy content = do
  dirs <- serializeDirectives ctx (getTypeDirectives ctx proxy)
  pure $
    TypeDefinition
      (visitTypeDescription ctx proxy Nothing)
      (useTypename ctx proxy)
      dirs
      content

deriveTypeGuardUnions :: (DERIVE_TYPE gql a) => TypeProxy gql args OUT a -> GQLResult [TypeName]
deriveTypeGuardUnions prx = uncurry deriveTypeDefinition prx >>= toUnionNames . fst

toUnionNames :: TypeDefinition OUT CONST -> GQLResult [TypeName]
toUnionNames TypeDefinition {..} =
  case typeContent of
    DataUnion {unionMembers} -> pure $ toList $ memberName <$> unionMembers
    DataObject {} -> pure [typeName]
    _ -> throwError ("guarded type \"" <> msg typeName <> "\" must be an union or object")

deriveScalarDefinition ::
  (gql a) =>
  (CatType cat a -> ScalarDefinition) ->
  UseDeriving gql args ->
  CatType cat a ->
  GQLResult (GQLTypeNode cat)
deriveScalarDefinition f ctx p = (`GQLTypeNode` []) <$> toTypeDefinition ctx p (mkScalar p (f p))

deriveTypeDefinition ::
  (DERIVE_TYPE gql a) =>
  UseDeriving gql args ->
  CatType c a ->
  GQLResult (TypeDefinition c CONST, [GQLTypeNodeExtension])
deriveTypeDefinition ctx prx = do
  (content, ext) <- deriveType (fieldGRep ctx prx) prx >>= toTypeContent ctx prx
  (,ext) <$> toTypeDefinition ctx prx content

deriveInterfaceDefinition ::
  (DERIVE_TYPE gql a) =>
  UseDeriving gql args ->
  CatType OUT a ->
  GQLResult (TypeDefinition OUT CONST, [GQLTypeNodeExtension])
deriveInterfaceDefinition ctx proxy = do
  (typeDef, ext) <- deriveTypeDefinition ctx proxy
  (,ext) <$> mapTypeContent (fmap DataInterface . withObject (typeName typeDef)) typeDef

fieldGRep :: (UseGQLType ctx gql) => ctx -> CatType cat a -> GRepFun gql gql Proxy (GQLResult (ArgumentsDefinition CONST))
fieldGRep gql cat =
  GRepFun
    { grepTypename = useTypename gql . (`mapCat` cat),
      grepWrappers = useWrappers gql . (`mapCat` cat),
      grepFun = useDeriveFieldArgs gql . (`mapCat` cat)
    }
