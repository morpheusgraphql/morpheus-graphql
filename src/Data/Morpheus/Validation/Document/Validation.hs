{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}


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
  validateType (name, FinalDataType( dt@DataType { typeName , typeMeta, typeContent = DataObject { objectImplements , objectFields}  })) = do         
      interface <- traverse getInterfaceByKey objectImplements
      case concatMap (mustBeSubset objectFields) interface of
        [] -> pure $ Just (name, dt) 
        errors -> failure $ partialImplements typeName errors
  
  validateType (name, FinalDataType x) = pure $ Just (name, x)
  validateType _ = pure Nothing
  mustBeSubset
    :: DataObject -> (Name, DataObject) -> [(Key, Key, ImplementsError)]
  mustBeSubset objFields (typeName, interfaceFields) = concatMap
    checkField
    interfaceFields
   where
    checkField :: (Key, DataField) -> [(Key, Key, ImplementsError)]
    checkField (key, DataField { fieldType = interfaceT@TypeRef { typeConName = interfaceTypeName, typeWrappers = interfaceWrappers } })
      = case lookup key objFields of
        Just DataField { fieldType = objT@TypeRef { typeConName, typeWrappers } }
          | typeConName == interfaceTypeName && not
            (isWeaker typeWrappers interfaceWrappers)
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
