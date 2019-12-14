{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Execution.Document.Convert
  ( renderTHTypes
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )

--
-- MORPHEUS
import           Data.Morpheus.Error.Internal   ( internalError )
import           Data.Morpheus.Execution.Internal.Utils
                                                ( capital )
import           Data.Morpheus.Types.Internal.AST
                                                ( ArgsType(..)
                                                , DataField(..)
                                                , DataTypeContent(..)
                                                , DataType(..)
                                                , DataTypeKind(..)
                                                , OperationType(..)
                                                , ResolverKind(..)
                                                , TypeRef(..)
                                                , DataEnumValue(..)
                                                , sysTypes
                                                , ConsD(..)
                                                , GQLTypeD(..)
                                                , TypeD(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation )

renderTHTypes :: Bool -> [(Text, DataType)] -> Validation [GQLTypeD]
renderTHTypes namespace lib = traverse renderTHType lib
 where
  renderTHType :: (Text, DataType) -> Validation GQLTypeD
  renderTHType (tyConName, x) = generateType x
   where
    genArgsTypeName fieldName | namespace = sysName tyConName <> argTName
                              | otherwise = argTName
      where argTName = capital fieldName <> "Args"
    genArgumentType :: (Text, DataField) -> Validation [TypeD]
    genArgumentType (_        , DataField { fieldArgs = [] }) = pure []
    genArgumentType (fieldName, DataField { fieldArgs }     ) = pure
      [ TypeD
          { tName
          , tNamespace = []
          , tCons      = [ ConsD { cName   = sysName $ pack tName
                                 , cFields = map genField fieldArgs
                                 }
                         ]
          , tMeta      = Nothing
          }
      ]
      where tName = genArgsTypeName $ sysName fieldName
    -------------------------------------------
    genFieldTypeName = genTypeName
    ------------------------------
    --genTypeName :: Text -> Text
    genTypeName "String"                    = "Text"
    genTypeName "Boolean"                   = "Bool"
    genTypeName name | name `elem` sysTypes = "S" <> name
    genTypeName name                        = name
    ----------------------------------------
    sysName = unpack . genTypeName
    ---------------------------------------------------------------------------------------------
    genField :: (Text, DataField) -> DataField
    genField (_, field@DataField { fieldType = alias@TypeRef { typeConName } })
      = field { fieldType = alias { typeConName = genFieldTypeName typeConName }
              }
    ---------------------------------------------------------------------------------------------
    genResField :: (Text, DataField) -> DataField
    genResField (_, field@DataField { fieldName, fieldArgs, fieldType = alias@TypeRef { typeConName } })
      = field { fieldType     = alias { typeConName = ftName, typeArgs }
              , fieldArgsType
              }
     where
      ftName   = genFieldTypeName typeConName
      ---------------------------------------
      typeArgs = case typeContent <$> lookup typeConName lib of
        Just DataObject{} -> Just "m"
        Just DataUnion{}  -> Just "m"
        _                 -> Nothing
      -----------------------------------
      fieldArgsType = Just
        $ ArgsType { argsTypeName, resKind = getFieldType ftName }
       where
        argsTypeName | null fieldArgs = "()"
                     | otherwise = pack $ genArgsTypeName $ unpack fieldName
        --------------------------------------
        getFieldType key = case typeContent <$> lookup key lib of
          Nothing           -> ExternalResolver
          Just DataObject{} -> TypeVarResolver
          Just DataUnion{}  -> TypeVarResolver
          Just _            -> PlainResolver
    --------------------------------------------
    generateType dt@DataType { typeName, typeContent, typeMeta } = genType
      typeContent
     where
      genType (DataEnum tags) = pure GQLTypeD
        { typeD        = TypeD { tName      = sysName typeName
                               , tNamespace = []
                               , tCons      = map enumOption tags
                               , tMeta      = typeMeta
                               }
        , typeKindD    = KindEnum
        , typeArgD     = []
        , typeOriginal = (typeName, dt)
        }
       where
        enumOption DataEnumValue { enumName } =
          ConsD { cName = sysName enumName, cFields = [] }
      genType (DataScalar _) =
        internalError "Scalar Types should defined By Native Haskell Types"
      genType (DataInputUnion _) = internalError "Input Unions not Supported"
      genType (DataInputObject fields) = pure GQLTypeD
        { typeD        =
          TypeD
            { tName      = sysName typeName
            , tNamespace = []
            , tCons      = [ ConsD { cName   = sysName typeName
                                   , cFields = map genField fields
                                   }
                           ]
            , tMeta      = typeMeta
            }
        , typeKindD    = KindInputObject
        , typeArgD     = []
        , typeOriginal = (typeName, dt)
        }
      genType (DataObject fields) = do
        typeArgD <- concat <$> traverse genArgumentType fields
        pure GQLTypeD
          { typeD        =
            TypeD
              { tName      = sysName typeName
              , tNamespace = []
              , tCons      = [ ConsD { cName   = sysName typeName
                                     , cFields = map genResField fields
                                     }
                             ]
              , tMeta      = typeMeta
              }
          , typeKindD    = if typeName == "Subscription"
                             then KindObject (Just Subscription)
                             else KindObject Nothing
          , typeArgD
          , typeOriginal = (typeName, dt)
          }
      genType (DataUnion members) = do
        let tCons = map unionCon members
        pure GQLTypeD
          { typeD        = TypeD { tName      = unpack typeName
                                 , tNamespace = []
                                 , tCons
                                 , tMeta      = typeMeta
                                 }
          , typeKindD    = KindUnion
          , typeArgD     = []
          , typeOriginal = (typeName, dt)
          }
       where
        unionCon memberName = ConsD
          { cName
          , cFields = [ DataField
                          { fieldName     = pack $ "un" <> cName
                          , fieldType     = TypeRef { typeConName = pack utName
                                                    , typeArgs = Just "m"
                                                    , typeWrappers = []
                                                    }
                          , fieldMeta     = Nothing
                          , fieldArgs     = []
                          , fieldArgsType = Nothing
                          }
                      ]
          }
         where
          cName  = sysName typeName <> utName
          utName = sysName memberName
