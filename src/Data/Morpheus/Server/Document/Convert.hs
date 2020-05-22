{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Server.Document.Convert
  ( toTHDefinitions,
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
    singleton,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD,
    DataTypeKind (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    GQLTypeD (..),
    OUT,
    TRUE,
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    argumentsToFields,
    hasArguments,
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

toTHDefinitions :: Bool -> [TypeDefinition ANY] -> Q [GQLTypeD]
toTHDefinitions namespace lib = catMaybes <$> traverse renderTHType lib
  where
    renderTHType :: TypeDefinition ANY -> Q (Maybe GQLTypeD)
    renderTHType x = generateType x
      where
        genArgsTypeName :: FieldName -> TypeName
        genArgsTypeName fieldName
          | namespace = hsTypeName (typeName x) <> argTName
          | otherwise = argTName
          where
            argTName = capitalTypeName (fieldName <> "Args")
        ---------------------------------------------------------------------------------------------
        genResField :: FieldDefinition OUT -> Q (FieldDefinition OUT)
        genResField field@FieldDefinition {fieldName, fieldArgs, fieldType = typeRef@TypeRef {typeConName}} =
          do
            typeArgs <- getTypeArgs typeConName lib
            pure $
              field
                { fieldType = typeRef {typeConName = hsTypeName typeConName, typeArgs},
                  fieldArgs = fieldArguments
                }
          where
            fieldArguments
              | hasArguments fieldArgs = fieldArgs {argumentsTypename = Just $ genArgsTypeName fieldName}
              | otherwise = fieldArgs
        --------------------------------------------
        generateType :: TypeDefinition ANY -> Q (Maybe GQLTypeD)
        generateType typeOriginal@TypeDefinition {typeName, typeContent, typeMeta} =
          genType
            typeContent
          where
            buildType :: [ConsD] -> TypeD
            buildType tCons =
              TypeD
                { tName = hsTypeName typeName,
                  tMeta = typeMeta,
                  tNamespace = [],
                  tCons,
                  tKind
                }
            buildObjectCons :: FieldsDefinition cat -> [ConsD]
            buildObjectCons fields = [mkCons typeName fields]
            tKind = kindOf typeOriginal
            -----------------------
            defType typeArgD cons =
              pure $
                Just
                  GQLTypeD
                    { typeD = buildType cons,
                      typeArgD,
                      ..
                    }
            genType :: TypeContent TRUE ANY -> Q (Maybe GQLTypeD)
            genType DataScalar {} = pure Nothing
            genType (DataEnum tags) = defType [] $ map mkConsEnum tags
            genType (DataInputObject fields) = defType [] $ buildObjectCons fields
            genType DataInputUnion {} = fail "Input Unions not Supported"
            genType DataInterface {interfaceFields} = do
              typeArgD <- concat <$> traverse (genArgumentType genArgsTypeName) (elems interfaceFields)
              objCons <- buildObjectCons <$> traverse genResField interfaceFields
              defType typeArgD objCons
            genType DataObject {objectFields} = do
              typeArgD <- concat <$> traverse (genArgumentType genArgsTypeName) (elems objectFields)
              objCons <- buildObjectCons <$> traverse genResField objectFields
              defType typeArgD objCons
            genType (DataUnion members) = defType [] $ map unionCon members
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
                            fieldMeta = Nothing,
                            fieldArgs = NoArguments
                          }
                    )
                  where
                    cName = hsTypeName typeName <> utName
                    utName = hsTypeName memberName

genArgumentType :: (FieldName -> TypeName) -> FieldDefinition OUT -> Q [TypeD]
genArgumentType _ FieldDefinition {fieldArgs = NoArguments} = pure []
genArgumentType namespaceWith FieldDefinition {fieldName, fieldArgs} =
  pure
    [ TypeD
        { tName,
          tNamespace = [],
          tCons =
            [ mkCons tName (argumentsToFields fieldArgs)
            ],
          tMeta = Nothing,
          tKind = KindInputObject
        }
    ]
  where
    tName = hsTypeName (namespaceWith fieldName)
