{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Transform
  ( toTHDefinitions,
    TypeDec (..),
  )
where

import Data.Morpheus.Internal.Utils
  ( capitalTypeName,
    elems,
    empty,
    mapSnd,
    singleton,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerConsD,
    ServerFieldDefinition,
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.Internal.TH.Utils (isParametrizedResolverType)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    IN,
    OUT,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    UnionMember (..),
    hsTypeName,
    kindOf,
    mkConsEnum,
    toFieldName,
  )
import Language.Haskell.TH
import Relude hiding (empty)

data TypeDec s = InputType (ServerTypeDefinition IN s) | OutputType (ServerTypeDefinition OUT s)

toTHDefinitions ::
  forall s.
  Bool ->
  [TypeDefinition ANY s] ->
  Q [TypeDec s]
toTHDefinitions namespace schema = traverse generateType schema
  where
    --------------------------------------------
    generateType :: TypeDefinition ANY s -> Q (TypeDec s)
    generateType
      typeDef@TypeDefinition
        { typeName,
          typeContent
        } =
        withType <$> genTypeContent schema toArgsTypeName typeName typeContent
        where
          toArgsTypeName :: FieldName -> TypeName
          toArgsTypeName = mkArgsTypeName namespace typeName
          tKind = kindOf typeDef
          typeOriginal = Just typeDef
          -------------------------
          withType (ConsIN tCons) =
            InputType
              ServerTypeDefinition
                { tName = hsTypeName typeName,
                  tCons,
                  typeArgD = empty,
                  ..
                }
          withType (ConsOUT typeArgD tCons) =
            OutputType
              ServerTypeDefinition
                { tName = hsTypeName typeName,
                  tCons,
                  ..
                }

toHSFieldDefinition :: FieldDefinition cat s -> FieldDefinition cat s
toHSFieldDefinition field = field {fieldType = toHSTypeRef (fieldType field)}

toHSTypeRef :: TypeRef -> TypeRef
toHSTypeRef tyRef@TypeRef {typeConName} = tyRef {typeConName = hsTypeName typeConName}

mkCons :: TypeName -> [ServerFieldDefinition cat s] -> ServerConsD cat s
mkCons typename fields =
  ConsD
    { cName = hsTypeName typename,
      cFields = fmap (mapSnd toHSFieldDefinition) fields
    }

mkObjectCons :: TypeName -> [ServerFieldDefinition cat s] -> [ServerConsD cat s]
mkObjectCons typeName fields = [mkCons typeName fields]

mkArgsTypeName :: Bool -> TypeName -> FieldName -> TypeName
mkArgsTypeName namespace typeName fieldName
  | namespace = hsTypeName typeName <> argTName
  | otherwise = argTName
  where
    argTName = capitalTypeName (fieldName <> "Args")

mkObjectField ::
  [TypeDefinition ANY s] ->
  (FieldName -> TypeName) ->
  FieldDefinition OUT s ->
  Q (ServerFieldDefinition OUT s)
mkObjectField
  schema
  genArgsTypeName
  FieldDefinition
    { fieldName,
      fieldContent = cont,
      fieldType = typeRef@TypeRef {typeConName},
      ..
    } =
    (,FieldDefinition
      { fieldName,
        fieldType = toHSTypeRef typeRef,
        fieldContent = cont >>= fieldCont,
        ..
      })
      <$> isParametrizedResolverType typeConName schema
    where
      fieldCont :: FieldContent TRUE OUT s -> Maybe (FieldContent TRUE OUT s)
      fieldCont (FieldArgs ArgumentsDefinition {arguments})
        | not (null arguments) =
          Just $ FieldArgs $
            ArgumentsDefinition
              { argumentsTypename = Just $ genArgsTypeName fieldName,
                arguments = arguments
              }
      fieldCont _ = Nothing

data BuildPlan s
  = ConsIN [ServerConsD IN s]
  | ConsOUT [ServerTypeDefinition IN s] [ServerConsD OUT s]

genTypeContent ::
  [TypeDefinition ANY s] ->
  (FieldName -> TypeName) ->
  TypeName ->
  TypeContent TRUE ANY s ->
  Q (BuildPlan s)
genTypeContent _ _ _ DataScalar {} = pure (ConsIN [])
genTypeContent _ _ _ (DataEnum tags) = pure $ ConsIN (fmap mkConsEnum tags)
genTypeContent _ _ typeName (DataInputObject fields) =
  pure $ ConsIN (mkObjectCons typeName $ map (False,) $ elems fields)
genTypeContent _ _ _ DataInputUnion {} = fail "Input Unions not Supported"
genTypeContent schema toArgsTyName typeName DataInterface {interfaceFields} = do
  typeArgD <- genArgumentTypes toArgsTyName interfaceFields
  objCons <- mkObjectCons typeName <$> traverse (mkObjectField schema toArgsTyName) (elems interfaceFields)
  pure $ ConsOUT typeArgD objCons
genTypeContent schema toArgsTyName typeName DataObject {objectFields} = do
  typeArgD <- genArgumentTypes toArgsTyName objectFields
  objCons <-
    mkObjectCons typeName
      <$> traverse (mkObjectField schema toArgsTyName) (elems objectFields)
  pure $ ConsOUT typeArgD objCons
genTypeContent _ _ typeName (DataUnion members) =
  pure $ ConsOUT [] (fmap unionCon members)
  where
    unionCon UnionMember {memberName} =
      mkCons
        cName
        ( singleton
            ( True,
              FieldDefinition
                { fieldName = "un" <> toFieldName cName,
                  fieldType =
                    TypeRef
                      { typeConName = utName,
                        typeWrappers = []
                      },
                  fieldDescription = Nothing,
                  fieldDirectives = empty,
                  fieldContent = Nothing
                }
            )
        )
      where
        cName = hsTypeName typeName <> utName
        utName = hsTypeName memberName

genArgumentTypes :: (FieldName -> TypeName) -> FieldsDefinition OUT s -> Q [ServerTypeDefinition IN s]
genArgumentTypes genArgsTypeName fields =
  concat <$> traverse (genArgumentType genArgsTypeName) (elems fields)

genArgumentType :: (FieldName -> TypeName) -> FieldDefinition OUT s -> Q [ServerTypeDefinition IN s]
genArgumentType namespaceWith FieldDefinition {fieldName, fieldContent = Just (FieldArgs ArgumentsDefinition {arguments})}
  | not (null arguments) =
    pure
      [ ServerTypeDefinition
          { tName,
            tCons = [mkCons tName $ map (False,) $ elems arguments],
            tKind = KindInputObject,
            typeArgD = [],
            typeOriginal = Nothing
          }
      ]
  where
    tName = hsTypeName (namespaceWith fieldName)
genArgumentType _ _ = pure []
