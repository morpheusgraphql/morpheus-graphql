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
import Data.Morpheus.Internal.TH
  ( infoTyVars,
    makeName,
  )
import Data.Morpheus.Internal.Utils
  ( capitalTypeName,
  )
import Data.Morpheus.Internal.Utils
  ( Listable (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD (..),
    DataEnumValue (..),
    DataTypeKind (..),
    FieldDefinition (..),
    FieldName,
    GQLTypeD (..),
    InputFieldsDefinition (..),
    TRUE,
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    hasArguments,
    hsTypeName,
    kindOf,
    lookupWith,
    toFieldName,
    toHSFieldDefinition,
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
  Nothing -> getTyArgs <$> reify (makeName key)

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
toTHDefinitions namespace lib = traverse renderTHType lib
  where
    renderTHType :: TypeDefinition ANY -> Q GQLTypeD
    renderTHType x = generateType x
      where
        genArgsTypeName :: FieldName -> TypeName
        genArgsTypeName fieldName
          | namespace = hsTypeName (typeName x) <> argTName
          | otherwise = argTName
          where
            argTName = capitalTypeName (fieldName <> "Args")
        ---------------------------------------------------------------------------------------------
        genResField :: FieldDefinition -> Q FieldDefinition
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
        generateType :: TypeDefinition ANY -> Q GQLTypeD
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
            buildObjectCons :: [FieldDefinition] -> [ConsD]
            buildObjectCons cFields =
              [ ConsD
                  { cName = hsTypeName typeName,
                    cFields
                  }
              ]
            tKind = kindOf typeOriginal
            genType :: TypeContent TRUE ANY -> Q GQLTypeD
            genType (DataEnum tags) =
              pure
                GQLTypeD
                  { typeD =
                      TypeD
                        { tName = hsTypeName typeName,
                          tNamespace = [],
                          tCons = map enumOption tags,
                          tMeta = typeMeta,
                          tKind
                        },
                    typeArgD = [],
                    ..
                  }
              where
                enumOption DataEnumValue {enumName} =
                  ConsD {cName = hsTypeName enumName, cFields = []}
            genType DataScalar {} = fail "Scalar Types should defined By Native Haskell Types"
            genType DataInputUnion {} = fail "Input Unions not Supported"
            genType DataInterface {interfaceFields} = do
              typeArgD <- concat <$> traverse (genArgumentType genArgsTypeName) (toList interfaceFields)
              objCons <- buildObjectCons <$> traverse genResField (toList interfaceFields)
              pure
                GQLTypeD
                  { typeD = buildType objCons,
                    typeArgD,
                    ..
                  }
            genType (DataInputObject fields) =
              pure
                GQLTypeD
                  { typeD = buildType $ buildObjectCons $ genInputFields fields,
                    typeArgD = [],
                    ..
                  }
            genType DataObject {objectFields} = do
              typeArgD <- concat <$> traverse (genArgumentType genArgsTypeName) (toList objectFields)
              objCons <- buildObjectCons <$> traverse genResField (toList objectFields)
              pure
                GQLTypeD
                  { typeD = buildType objCons,
                    typeArgD,
                    ..
                  }
            genType (DataUnion members) =
              pure
                GQLTypeD
                  { typeD = buildType (map unionCon members),
                    typeArgD = [],
                    ..
                  }
              where
                unionCon memberName =
                  ConsD
                    { cName,
                      cFields =
                        [ FieldDefinition
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
                        ]
                    }
                  where
                    cName = hsTypeName typeName <> utName
                    utName = hsTypeName memberName

genArgumentType :: (FieldName -> TypeName) -> FieldDefinition -> Q [TypeD]
genArgumentType _ FieldDefinition {fieldArgs = NoArguments} = pure []
genArgumentType namespaceWith FieldDefinition {fieldName, fieldArgs} =
  pure
    [ TypeD
        { tName,
          tNamespace = [],
          tCons =
            [ ConsD
                { cName = tName,
                  cFields = genArguments fieldArgs
                }
            ],
          tMeta = Nothing,
          tKind = KindInputObject
        }
    ]
  where
    tName = hsTypeName (namespaceWith fieldName)

genArguments :: ArgumentsDefinition -> [FieldDefinition]
genArguments = genInputFields . InputFieldsDefinition . arguments

genInputFields :: InputFieldsDefinition -> [FieldDefinition]
genInputFields = map toHSFieldDefinition . toList
