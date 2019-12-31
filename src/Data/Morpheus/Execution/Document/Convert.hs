{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Morpheus.Execution.Document.Convert
  ( toTHDefinitions
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      (unpack)
import           Language.Haskell.TH  

-- MORPHEUS
import           Data.Morpheus.Types.Internal.TH (infoTyVars)
import           Data.Morpheus.Execution.Internal.Utils
                                                ( capital )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataField(..)
                                                , DataTypeContent(..)
                                                , DataType(..)
                                                , DataTypeKind(..)
                                                , OperationType(..)
                                                , TypeRef(..)
                                                , DataEnumValue(..)
                                                , sysTypes
                                                , ConsD(..)
                                                , GQLTypeD(..)
                                                , TypeD(..)
                                                , Key
                                                , DataObject
                                                )


m_ :: Key
m_ = "m"

getTypeArgs :: Key -> [(Key, DataType)] -> Q (Maybe Key)
getTypeArgs "__TypeKind" _ = pure Nothing
getTypeArgs "Boolean" _ = pure Nothing
getTypeArgs "String" _ = pure Nothing
getTypeArgs "Int" _ = pure Nothing
getTypeArgs "Float" _ = pure Nothing
getTypeArgs key lib = case typeContent <$> lookup key lib of
  Just x  -> pure (kindToTyArgs x)
  Nothing -> getTyArgs <$> reify (mkName $ unpack key)

getTyArgs :: Info -> Maybe Key
getTyArgs x 
          | length (infoTyVars x) > 0 = Just m_ 
          | otherwise = Nothing


kindToTyArgs :: DataTypeContent -> Maybe Key
kindToTyArgs DataObject{} = Just m_
kindToTyArgs DataUnion{}  = Just m_
kindToTyArgs _             = Nothing

toTHDefinitions :: Bool -> [(Key, DataType)] -> Q [GQLTypeD]
toTHDefinitions namespace lib = traverse renderTHType lib
 where
  renderTHType :: (Key, DataType) -> Q GQLTypeD
  renderTHType (tyConName, x) = generateType x
   where
    genArgsTypeName :: Key -> Key
    genArgsTypeName fieldName | namespace = hsTypeName tyConName <> argTName
                              | otherwise = argTName
      where argTName = capital fieldName <> "Args"
    ---------------------------------------------------------------------------------------------
    genResField :: (Key, DataField) -> Q DataField
    genResField (_, field@DataField { fieldName, fieldArgs, fieldType = typeRef@TypeRef { typeConName } })
      = do 
        typeArgs <- getTypeArgs typeConName lib 
        pure $ field { 
                fieldType = typeRef { typeConName = hsTypeName typeConName, typeArgs }
                , fieldArgsType
              }
     where
      fieldArgsType
        | null fieldArgs = Nothing
        | otherwise = Just (genArgsTypeName fieldName)
    --------------------------------------------
    generateType :: DataType -> Q GQLTypeD
    generateType dt@DataType { typeName, typeContent, typeMeta } = genType
      typeContent
     where
      genType :: DataTypeContent -> Q GQLTypeD
      genType (DataEnum tags) = pure GQLTypeD
        { typeD        = TypeD { tName      = hsTypeName typeName
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
          ConsD { cName = hsTypeName enumName, cFields = [] }
      genType DataScalar {} = fail "Scalar Types should defined By Native Haskell Types"
      genType DataInputUnion {} = fail "Input Unions not Supported"
      genType DataInterface {} = fail "interfaces must be eliminated in Validation"
      genType (DataInputObject fields) = pure GQLTypeD
        { typeD        =
          TypeD
            { tName      = hsTypeName typeName
            , tNamespace = []
            , tCons      = [ ConsD { cName   = hsTypeName typeName
                                   , cFields = genInputFields fields
                                   }
                           ]
            , tMeta      = typeMeta
            }
        , typeKindD    = KindInputObject
        , typeArgD     = []
        , typeOriginal = (typeName, dt)
        }
      genType DataObject {objectFields} = do
        typeArgD <- concat <$> traverse (genArgumentType genArgsTypeName) objectFields
        cFields  <- traverse genResField objectFields
        pure GQLTypeD
          { typeD        = TypeD
                             { tName      = hsTypeName typeName
                             , tNamespace = []
                             , tCons = [ ConsD { cName = hsTypeName typeName
                                               , cFields 
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
          { typeD        = TypeD { tName      = typeName
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
                          { fieldName     = "un" <> cName
                          , fieldType     = TypeRef { typeConName  = utName
                                                    , typeArgs     = Just m_
                                                    , typeWrappers = []
                                                    }
                          , fieldMeta     = Nothing
                          , fieldArgs     = []
                          , fieldArgsType = Nothing
                          }
                      ]
          }
         where
          cName  = hsTypeName typeName <> utName
          utName = hsTypeName memberName



hsTypeName :: Key -> Key
hsTypeName "String"                    = "Text"
hsTypeName "Boolean"                   = "Bool"
hsTypeName name | name `elem` sysTypes = "S" <> name
hsTypeName name                        = name

genArgumentType :: (Key -> Key) -> (Key, DataField) -> Q [TypeD]
genArgumentType _ (_, DataField { fieldArgs = [] }) = pure []
genArgumentType namespaceWith (fieldName, DataField { fieldArgs }) = pure
  [ TypeD
      { tName
      , tNamespace = []
      , tCons      = [ ConsD { cName   = hsTypeName tName
                             , cFields = genInputFields fieldArgs
                             }
                     ]
      , tMeta      = Nothing
      }
  ]
  where tName = namespaceWith (hsTypeName fieldName)


genInputFields :: DataObject -> [DataField]
genInputFields = map (genField . snd)
 where
  genField :: DataField -> DataField
  genField field@DataField { fieldType = tyRef@TypeRef { typeConName } } =
    field { fieldType = tyRef { typeConName = hsTypeName typeConName } }
