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
                                                , TypeRef(..)
                                                , isWeaker
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )

validatePartialDocument :: [(Key, DataType)] -> Validation [(Key, DataType)]
validatePartialDocument lib = catMaybes <$> traverse validateType lib
 where
  validateType :: (Key, DataType) -> Validation (Maybe (Key, DataType))
  validateType (name, dt@DataType { typeName , typeContent = DataObject { objectImplements , objectFields}  }) = do         
      interface <- traverse getInterfaceByKey objectImplements
      case concatMap (mustBeSubset objectFields) interface of
        [] -> pure $ Just (name, dt) 
        errors -> failure $ partialImplements typeName errors
  validateType (_,DataType { typeContent = DataInterface {}}) = pure Nothing
  validateType (name, x) = pure $ Just (name, x)
  mustBeSubset
    :: DataObject -> (Name, DataObject) -> [(Key, Key, ImplementsError)]
  mustBeSubset objFields (typeName, interfaceFields ) = concatMap
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
    Just DataType { typeContent = DataInterface { interfaceFields } } -> pure (key,interfaceFields)
    _ -> failure $ unknownInterface key
