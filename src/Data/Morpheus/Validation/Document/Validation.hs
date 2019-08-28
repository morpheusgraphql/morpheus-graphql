{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument
  ) where

import           Data.Maybe

--
-- Morpheus
import           Data.Morpheus.Error.Document.Interface  (ImplementsError (..), partialImplements, unknownInterface)
import           Data.Morpheus.Types.Internal.Base       (Location (..))
import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), DataOutputField, DataOutputObject,
                                                          DataType (..), Key, RawDataType (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)

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
    mustImplement :: DataOutputObject -> [Key] -> Validation DataFullType
    mustImplement object interfaceKey = do
      interface <- traverse getInterfaceByKey interfaceKey
      case concatMap (mustBeSubset object) interface of
        []     -> pure $ OutputObject object
        errors -> Left $ partialImplements (typeName object) position errors
    -------------------------------
    mustBeSubset :: DataOutputObject -> DataOutputObject -> [(Key, Key, ImplementsError)]
    mustBeSubset DataType {typeData = objFields} DataType {typeName, typeData = interfaceFields} =
      concatMap checkField interfaceFields
      where
        checkField :: (Key, DataOutputField) -> [(Key, Key, ImplementsError)]
        checkField (key, _) =
          case lookup key objFields of
            Just _  -> []
            Nothing -> [(typeName, key, UndefinedField)]
    -------------------------------
    position = Location 0 0 -- TODO
    getInterfaceByKey :: Key -> Validation DataOutputObject
    getInterfaceByKey key =
      case lookup key lib of
        Just (Interface x) -> pure x
        _                  -> Left $ unknownInterface key position
