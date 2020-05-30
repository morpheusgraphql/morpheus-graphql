{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Server.Document.Transform
  ( toTHDefinitions,
    TypeDec (..),
  )
where

-- MORPHEUS

import Data.Maybe (catMaybes)
import Data.Morpheus.Internal.TH
  ( infoTyVars,
    mkTypeName,
  )
import Data.Morpheus.Internal.Utils
  ( capitalTypeName,
    elems,
    empty,
    singleton,
  )
import Data.Morpheus.Server.Internal.TH.Types (TypeD (..))
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    Fields (..),
    FieldsDefinition,
    IN,
    OUT,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    hsTypeName,
    kindOf,
    lookupWith,
    mkCons,
    mkConsEnum,
    toFieldName,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH

m_ :: TypeName
m_ = "m"

getTypeArgs :: TypeName -> [TypeDefinition ANY] -> Q (Maybe TypeName)
getTypeArgs "__TypeKind" _ = pure Nothing
getTypeArgs "Boolean" _ = pure Nothing
getTypeArgs "String" _ = pure Nothing
getTypeArgs "Int" _ = pure Nothing
getTypeArgs "Float" _ = pure Nothing
getTypeArgs key lib = case typeContent <$> lookupWith typeName key lib of
  Just x -> pure (kindToTyArgs x)
  Nothing -> getTyArgs <$> reify (mkTypeName key)

getTyArgs :: Info -> Maybe TypeName
getTyArgs x
  | null (infoTyVars x) = Nothing
  | otherwise = Just m_

kindToTyArgs :: TypeContent TRUE ANY -> Maybe TypeName
kindToTyArgs DataObject {} = Just m_
kindToTyArgs DataUnion {} = Just m_
kindToTyArgs DataInterface {} = Just m_
kindToTyArgs _ = Nothing

data TypeDec = InputType (TypeD IN) | OutputType (TypeD OUT)

toTHDefinitions :: Bool -> [TypeDefinition ANY] -> Q [TypeDec]
toTHDefinitions namespace schema = catMaybes <$> traverse renderTHType schema
  where
    renderTHType :: TypeDefinition ANY -> Q (Maybe TypeDec)
    renderTHType x = generateType x
      where
        toArgsTypeName :: FieldName -> TypeName
        toArgsTypeName = mkArgsTypeName namespace (typeName x)
        --------------------------------------------
        generateType :: TypeDefinition ANY -> Q (Maybe TypeDec)
        generateType
          typeOriginal@TypeDefinition
            { typeName,
              typeContent,
              typeDescription
            } =
            withType <$> genTypeContent schema toArgsTypeName typeName typeContent
            where
              withType NoBuilds = Nothing
              withType (ConsIN tCons) =
                Just $
                  InputType
                    TypeD
                      { tName = hsTypeName typeName,
                        tDescription = typeDescription,
                        tNamespace = [],
                        tCons,
                        tKind = kindOf typeOriginal,
                        typeArgD = empty,
                        ..
                      }
              withType (ConsOUT typeArgD tCons) =
                Just $
                  OutputType
                    TypeD
                      { tName = hsTypeName typeName,
                        tDescription = typeDescription,
                        tNamespace = [],
                        tCons,
                        tKind = kindOf typeOriginal,
                        ..
                      }

mkObjectCons :: TypeName -> FieldsDefinition cat -> [ConsD cat]
mkObjectCons typeName fields = [mkCons typeName fields]

mkArgsTypeName :: Bool -> TypeName -> FieldName -> TypeName
mkArgsTypeName namespace typeName fieldName
  | namespace = hsTypeName typeName <> argTName
  | otherwise = argTName
  where
    argTName = capitalTypeName (fieldName <> "Args")

mkObjectField :: [TypeDefinition ANY] -> (FieldName -> TypeName) -> FieldDefinition OUT -> Q (FieldDefinition OUT)
mkObjectField schema genArgsTypeName FieldDefinition {fieldName, fieldContent = cont, fieldType = typeRef@TypeRef {typeConName}, ..} =
  do
    typeArgs <- getTypeArgs typeConName schema
    pure
      FieldDefinition
        { fieldName,
          fieldType = typeRef {typeConName = hsTypeName typeConName, typeArgs},
          fieldContent = fieldCont cont,
          ..
        }
  where
    fieldCont :: FieldContent TRUE OUT -> FieldContent TRUE OUT
    fieldCont (FieldArgs ArgumentsDefinition {arguments}) =
      FieldArgs $
        ArgumentsDefinition
          { argumentsTypename = Just $ genArgsTypeName fieldName,
            arguments = arguments
          }
    fieldCont _ = NoContent

data BuildPlan
  = NoBuilds
  | ConsIN [ConsD IN]
  | ConsOUT [TypeD IN] [ConsD OUT]

genTypeContent ::
  [TypeDefinition ANY] ->
  (FieldName -> TypeName) ->
  TypeName ->
  TypeContent TRUE ANY ->
  Q BuildPlan
genTypeContent _ _ _ DataScalar {} = pure NoBuilds
genTypeContent _ _ _ (DataEnum tags) = pure $ ConsIN (map mkConsEnum tags)
genTypeContent _ _ typeName (DataInputObject fields) =
  pure $ ConsIN (mkObjectCons typeName fields)
genTypeContent _ _ _ DataInputUnion {} = fail "Input Unions not Supported"
genTypeContent schema toArgsTyName typeName DataInterface {interfaceFields} = do
  typeArgD <- genArgumentTypes toArgsTyName interfaceFields
  objCons <- mkObjectCons typeName <$> traverse (mkObjectField schema toArgsTyName) interfaceFields
  pure $ ConsOUT typeArgD objCons
genTypeContent schema toArgsTyName typeName DataObject {objectFields} = do
  typeArgD <- genArgumentTypes toArgsTyName objectFields
  objCons <-
    mkObjectCons typeName
      <$> traverse (mkObjectField schema toArgsTyName) objectFields
  pure $ ConsOUT typeArgD objCons
genTypeContent _ _ typeName (DataUnion members) =
  pure $ ConsOUT [] (map unionCon members)
  where
    unionCon memberName =
      mkCons
        cName
        ( singleton
            FieldDefinition
              { fieldName = "un" <> toFieldName cName,
                fieldType =
                  TypeRef
                    { typeConName = utName,
                      typeArgs = Just m_,
                      typeWrappers = []
                    },
                fieldDescription = Nothing,
                fieldDirectives = empty,
                fieldContent = NoContent
              }
        )
      where
        cName = hsTypeName typeName <> utName
        utName = hsTypeName memberName

genArgumentTypes :: (FieldName -> TypeName) -> FieldsDefinition OUT -> Q [TypeD IN]
genArgumentTypes genArgsTypeName fields =
  concat <$> traverse (genArgumentType genArgsTypeName) (elems fields)

genArgumentType :: (FieldName -> TypeName) -> FieldDefinition OUT -> Q [TypeD IN]
genArgumentType namespaceWith FieldDefinition {fieldName, fieldContent = FieldArgs ArgumentsDefinition {arguments}} =
  pure
    [ TypeD
        { tName,
          tNamespace = empty,
          tCons = [mkCons tName (Fields arguments)],
          tDescription = Nothing,
          tKind = KindInputObject
        }
    ]
  where
    tName = hsTypeName (namespaceWith fieldName)
genArgumentType _ _ = pure []
