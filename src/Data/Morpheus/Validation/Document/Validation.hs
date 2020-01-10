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
                                                ( Name
                                                , FieldDefinition(..)
                                                , DataType(..)
                                                , FieldsDefinition(..)
                                                , DataTypeContent(..)
                                                , TypeRef(..)
                                                , Listable(..)
                                                , isWeaker
                                                , lookupWith
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )

validatePartialDocument :: [DataType] -> Validation [DataType]
validatePartialDocument lib = catMaybes <$> traverse validateType lib
 where
  validateType :: DataType -> Validation (Maybe DataType)
  validateType dt@DataType { typeName , typeContent = DataObject { objectImplements , objectFields}  } = do         
      interface <- traverse getInterfaceByKey objectImplements
      case concatMap (mustBeSubset objectFields) interface of
        [] -> pure (Just dt) 
        errors -> failure $ partialImplements typeName errors
  validateType DataType { typeContent = DataInterface {}} = pure Nothing
  validateType x = pure (Just x)
  mustBeSubset
    :: FieldsDefinition -> (Name, FieldsDefinition) -> [(Name, Name, ImplementsError)]
  mustBeSubset objFields (typeName, fields) = concatMap checkField (toList fields)
   where
    checkField :: (Name, FieldDefinition) -> [(Name, Name, ImplementsError)]
    checkField (key, FieldDefinition { fieldType = interfaceT@TypeRef { typeConName = interfaceTypeName, typeWrappers = interfaceWrappers } })
      = case lookup key (toList objFields) of
        Just FieldDefinition { fieldType = objT@TypeRef { typeConName, typeWrappers } }
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
  getInterfaceByKey :: Name -> Validation (Name, FieldsDefinition)
  getInterfaceByKey key = case lookupWith typeName key lib of
    Just DataType { typeContent = DataInterface { interfaceFields } } -> pure (key,interfaceFields)
    _ -> failure $ unknownInterface key
