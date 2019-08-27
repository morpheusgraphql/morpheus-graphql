module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument
  ) where

import           Data.Maybe

--
-- Morpheus
import           Data.Morpheus.Error.Document.Interface  (unknownInterface)
import           Data.Morpheus.Types.Internal.Base       (Location (..))
import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), DataOutputObject, Key, RawDataType (..))
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
      if all (isSubset object) interface
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
        _                  -> Left $ unknownInterface key $ Location 0 0 -- TODO
