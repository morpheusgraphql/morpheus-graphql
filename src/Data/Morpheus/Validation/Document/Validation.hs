{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument
  )
where

import           Data.Maybe

--
-- Morpheus
import           Data.Morpheus.Error.Document.Interface
                                                ( ImplementsError(..)
                                                , partialImplements
                                                , unknownInterface
                                                )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataField(..)
                                                , DataType(..)
                                                , DataObject
                                                , DataTypeContent(..)
                                                , Name
                                                , Key
                                                , RawDataType(..)
                                                , TypeRef(..)
                                                , DataFingerprint(..)
                                                , Meta
                                                , isWeaker
                                                , isWeaker
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )

validatePartialDocument :: [(Key, RawDataType)] -> Validation [(Key, DataType)]
validatePartialDocument lib = catMaybes <$> traverse validateType lib
 where
  validateType :: (Key, RawDataType) -> Validation (Maybe (Key, DataType))
  validateType (name, FinalDataType x) = pure $ Just (name, x)
  validateType (name, Implements { implementsName, implementsInterfaces, implementsMeta, implementsContent })
    = asTuple name
      <$>             (implementsName, implementsMeta, implementsContent)
      `mustImplement` implementsInterfaces
  validateType _ = pure Nothing
  -----------------------------------
  asTuple name x = Just (name, x)
  -----------------------------------
  mustImplement :: (Name, Maybe Meta, DataObject) -> [Key] -> Validation DataType
  mustImplement (typeName, typeMeta, object) interfaceKey = do
    interface <- traverse getInterfaceByKey interfaceKey
    case concatMap (mustBeSubset object) interface of
      [] -> pure $ DataType { typeName
                            , typeFingerprint = DataFingerprint typeName []
                            , typeMeta
                            , typeContent     = DataObject object
                            }
      errors -> failure $ partialImplements typeName errors
  -------------------------------
  mustBeSubset
    :: DataObject -> (Name, DataObject) -> [(Key, Key, ImplementsError)]
  mustBeSubset objFields (typeName, interfaceFields) = concatMap
    checkField
    interfaceFields
   where
    checkField :: (Key, DataField) -> [(Key, Key, ImplementsError)]
    checkField (key, DataField { fieldType = interfaceT@TypeRef { aliasTyCon = interfaceTypeName, aliasWrappers = interfaceWrappers } })
      = case lookup key objFields of
        Just DataField { fieldType = objT@TypeRef { aliasTyCon, aliasWrappers } }
          | aliasTyCon == interfaceTypeName && not
            (isWeaker aliasWrappers interfaceWrappers)
          -> []
          | otherwise
          -> [ ( typeName
               , key
               , UnexpectedType { expectedType = render interfaceT
                                , foundType    = render objT
                                }
               )
             ]
        Nothing -> [(typeName, key, UndefinedField)]
  -------------------------------
  getInterfaceByKey :: Key -> Validation (Name,DataObject)
  getInterfaceByKey key = case lookup key lib of
    Just Interface { interfaceContent } -> pure (key,interfaceContent)
    _ -> failure $ unknownInterface key
