module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument
  ) where

import           Data.Maybe
import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), Key, RawDataType (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)

validatePartialDocument :: [(Key, RawDataType)] -> Validation [(Key, DataFullType)]
validatePartialDocument = fmap catMaybes . traverse validateType
  where
    validateType :: (Key, RawDataType) -> Validation (Maybe (Key, DataFullType))
    validateType (name, FinalDataType x) = pure $ Just (name, x)
    validateType (name, Implements _ x)  = pure $ Just (name, OutputObject x)
    validateType _                       = pure Nothing
