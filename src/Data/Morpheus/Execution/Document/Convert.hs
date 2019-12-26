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

--
-- MORPHEUS
import           Data.Morpheus.Error.Internal   ( internalError )
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
                                                , Name
                                                , DataObject
                                                , kindOf
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation )


m_ :: Name
m_ = "m"

getFieldKind :: Name -> [(Name, DataType)] -> Validation DataTypeKind
getFieldKind key lib = pure $ case lookup key lib of
  Just x  -> kindOf x
  -- Nothing           -> ExternalResolver


renderTHTypes :: Bool -> [(Name, DataType)] -> Validation [GQLTypeD]
renderTHTypes namespace lib = traverse renderTHType lib
 where
  renderTHType :: (Name, DataType) -> Validation GQLTypeD
  renderTHType (tyConName, x) = generateType x
   where
    genArgsTypeName :: Name -> Name
    genArgsTypeName fieldName | namespace = hsTypeName tyConName <> argTName
                              | otherwise = argTName
      where argTName = capital fieldName <> "Args"
    ---------------------------------------------------------------------------------------------
    genResField :: (Name, DataField) -> DataField
    genResField (_, field@DataField { fieldName, fieldArgs, fieldType = typeRef@TypeRef { typeConName } })
      = field { fieldType     = typeRef { typeConName = ftName, typeArgs }
              , fieldArgsType
              }
     where
      ftName   = hsTypeName typeConName
      ---------------------------------------
      typeArgs = case typeContent <$> lookup typeConName lib of
        Just DataObject{} -> Just m_
        Just DataUnion{}  -> Just m_
        _                 -> Nothing
      -----------------------------------
      fieldArgsType
        | null fieldArgs = Nothing
        | otherwise = Just (genArgsTypeName fieldName)
    --------------------------------------------
    generateType dt@DataType { typeName, typeContent, typeMeta } = genType
      typeContent
     where
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
      genType (DataScalar _) =
        internalError "Scalar Types should defined By Native Haskell Types"
      genType (DataInputUnion _) = internalError "Input Unions not Supported"
      genType (DataInputObject fields) = pure GQLTypeD
        { typeD        =
          TypeD
            { tName      = hsTypeName typeName
            , tNamespace = []
            , tCons      = [ ConsD { cName   = hsTypeName typeName
                                   , cFields = genFields fields
                                   }
                           ]
            , tMeta      = typeMeta
            }
        , typeKindD    = KindInputObject
        , typeArgD     = []
        , typeOriginal = (typeName, dt)
        }
      genType (DataObject fields) = do
        typeArgD <- concat <$> traverse (genArgumentType genArgsTypeName) fields
        pure GQLTypeD
          { typeD        = TypeD
                             { tName      = hsTypeName typeName
                             , tNamespace = []
                             , tCons = [ ConsD { cName = hsTypeName typeName
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



hsTypeName :: Name -> Name
hsTypeName "String"                    = "Text"
hsTypeName "Boolean"                   = "Bool"
hsTypeName name | name `elem` sysTypes = "S" <> name
hsTypeName name                        = name

genArgumentType :: (Name -> Name) -> (Name, DataField) -> Validation [TypeD]
genArgumentType _ (_, DataField { fieldArgs = [] }) = pure []
genArgumentType namespaceWith (fieldName, DataField { fieldArgs }) = pure
  [ TypeD
      { tName
      , tNamespace = []
      , tCons      = [ ConsD { cName   = hsTypeName tName
                             , cFields = genFields fieldArgs
                             }
                     ]
      , tMeta      = Nothing
      }
  ]
  where tName = namespaceWith (hsTypeName fieldName)


genFields :: DataObject -> [DataField]
genFields = map (genField . snd)
 where
  genField :: DataField -> DataField
  genField field@DataField { fieldType = tyRef@TypeRef { typeConName } } =
    field { fieldType = tyRef { typeConName = hsTypeName typeConName } }
