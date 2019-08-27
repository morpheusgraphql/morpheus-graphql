module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument
  ) where

import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), Key, RawDataType (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)

validatePartialDocument :: [(Key, RawDataType)] -> Validation [(Key, DataFullType)]
validatePartialDocument [] = pure []
validatePartialDocument ((name, FinalDataType x):xs) = do
  list <- validatePartialDocument xs
  pure $ (name, x) : list
validatePartialDocument (_:xs) = validatePartialDocument xs
