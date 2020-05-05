{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Server.Document.Convert
  ( toTHDefinitions,
  )
where

-- MORPHEUS

import Data.Morpheus.Internal.TH (infoTyVars)
import Data.Morpheus.Internal.Utils
  ( capital,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    ConsD (..),
    DataEnumValue (..),
    FieldDefinition (..),
    GQLTypeD (..),
    InputFieldsDefinition (..),
    Key,
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeRef (..),
    hasArguments,
    hsTypeName,
    kindOf,
    lookupWith,
    toHSFieldDefinition,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Listable (..),
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Language.Haskell.TH

m_ :: Key
m_ = "m"

getTypeArgs :: Key -> [TypeDefinition] -> Q (Maybe Key)
getTypeArgs "__TypeKind" _ = pure Nothing
getTypeArgs "Boolean" _ = pure Nothing
getTypeArgs "String" _ = pure Nothing
getTypeArgs "Int" _ = pure Nothing
getTypeArgs "Float" _ = pure Nothing
getTypeArgs key lib = case typeContent <$> lookupWith typeName key lib of
  Just x -> pure (kindToTyArgs x)
  Nothing -> getTyArgs <$> reify (mkName $ unpack key)

getTyArgs :: Info -> Maybe Key
getTyArgs x
  | null (infoTyVars x) = Nothing
  | otherwise = Just m_

kindToTyArgs :: TypeContent -> Maybe Key
kindToTyArgs DataObject {} = Just m_
kindToTyArgs DataUnion {} = Just m_
kindToTyArgs _ = Nothing

toTHDefinitions :: Bool -> [TypeDefinition] -> Q [GQLTypeD]
toTHDefinitions namespace lib = traverse renderTHType lib
  where
    renderTHType :: TypeDefinition -> Q GQLTypeD
    renderTHType x = generateType x
      where
        genArgsTypeName :: Key -> Key
        genArgsTypeName fieldName
          | namespace = hsTypeName (typeName x) <> argTName
          | otherwise = argTName
          where
            argTName = capital fieldName <> "Args"
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
        generateType :: TypeDefinition -> Q GQLTypeD
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
                  tCons
                }
            buildObjectCons :: [FieldDefinition] -> [ConsD]
            buildObjectCons cFields =
              [ ConsD
                  { cName = hsTypeName typeName,
                    cFields
                  }
              ]
            typeKindD = kindOf typeOriginal
            genType :: TypeContent -> Q GQLTypeD
            genType (DataEnum tags) =
              pure
                GQLTypeD
                  { typeD =
                      TypeD
                        { tName = hsTypeName typeName,
                          tNamespace = [],
                          tCons = map enumOption tags,
                          tMeta = typeMeta
                        },
                    typeArgD = [],
                    ..
                  }
              where
                enumOption DataEnumValue {enumName} =
                  ConsD {cName = hsTypeName enumName, cFields = []}
            genType DataScalar {} = fail "Scalar Types should defined By Native Haskell Types"
            genType DataInputUnion {} = fail "Input Unions not Supported"
            genType DataInterface {} = fail "interfaces must be eliminated in Validation"
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
                            { fieldName = "un" <> cName,
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

genArgumentType :: (Key -> Key) -> FieldDefinition -> Q [TypeD]
genArgumentType _ FieldDefinition {fieldArgs = NoArguments} = pure []
genArgumentType namespaceWith FieldDefinition {fieldName, fieldArgs} =
  pure
    [ TypeD
        { tName,
          tNamespace = [],
          tCons =
            [ ConsD
                { cName = hsTypeName tName,
                  cFields = genArguments fieldArgs
                }
            ],
          tMeta = Nothing
        }
    ]
  where
    tName = namespaceWith (hsTypeName fieldName)

genArguments :: ArgumentsDefinition -> [FieldDefinition]
genArguments = genInputFields . InputFieldsDefinition . arguments

genInputFields :: InputFieldsDefinition -> [FieldDefinition]
genInputFields = map toHSFieldDefinition . toList
