{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument
  ) where

import           Data.Maybe

--
-- Morpheus
import           Data.Morpheus.Error.Document.Interface  (ImplementsError (..), partialImplements, unknownInterface)
import           Data.Morpheus.Types.Internal.Data       (DataField (..), DataFullType (..), DataObject, DataTyCon (..),
                                                          Key, RawDataType (..), showWrappedType)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Validation.Internal.Utils (isEqOrStricter)

validatePartialDocument :: [(Key, RawDataType)] -> Validation [(Key, DataFullType)]
validatePartialDocument lib = catMaybes <$> traverse validateType lib
  where
    validateType :: (Key, RawDataType) -> Validation (Maybe (Key, DataFullType))
    validateType (name, FinalDataType x)              = pure $ Just (name, x)
    validateType (name, Implements interfaces object) = asTuple name <$> object `mustImplement` interfaces
    validateType _                                    = pure Nothing
    -----------------------------------
    asTuple name x = Just (name, x)
    -----------------------------------
    mustImplement :: DataObject -> [Key] -> Validation DataFullType
    mustImplement object interfaceKey = do
      interface <- traverse getInterfaceByKey interfaceKey
      case concatMap (mustBeSubset object) interface of
        []     -> pure $ OutputObject object
        errors -> Left $ partialImplements (typeName object) errors
    -------------------------------
    mustBeSubset :: DataObject -> DataObject -> [(Key, Key, ImplementsError)]
    mustBeSubset DataTyCon {typeData = objFields} DataTyCon {typeName, typeData = interfaceFields} =
      concatMap checkField interfaceFields
      where
        checkField :: (Key, DataField) -> [(Key, Key, ImplementsError)]
        checkField (key, DataField {fieldType = interfaceTypeName, fieldTypeWrappers = interfaceWrappers}) =
          case lookup key objFields of
            Just DataField {fieldType, fieldTypeWrappers}
              | fieldType == interfaceTypeName && isEqOrStricter fieldTypeWrappers interfaceWrappers -> []
              | otherwise -> [(typeName, key, UnexpectedType {expectedType, foundType})]
              where expectedType = showWrappedType interfaceWrappers interfaceTypeName
                    foundType = showWrappedType fieldTypeWrappers fieldType
            Nothing -> [(typeName, key, UndefinedField)]
    -------------------------------
    getInterfaceByKey :: Key -> Validation DataObject
    getInterfaceByKey key =
      case lookup key lib of
        Just (Interface x) -> pure x
        _                  -> Left $ unknownInterface key
