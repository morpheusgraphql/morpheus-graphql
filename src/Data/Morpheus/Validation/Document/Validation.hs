module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument
  ) where

import           Data.Maybe
import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), DataInputObject, DataOutputObject,
                                                          DataType, Key, RawDataType (..))
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
      if foldl (&&) True (map (isSubset object) interface)
        then pure $ OutputObject object
        else fail "TODO"
    -------------------------------
    isSubset :: DataOutputObject -> DataOutputObject -> Bool
    isSubset _ _ = True
    -------------------------------
    getInterfaceByKey :: Key -> Validation DataOutputObject
    getInterfaceByKey key =
      case lookup key lib of
        Just (Interface x) -> pure x
        _                  -> fail "TDOD"
