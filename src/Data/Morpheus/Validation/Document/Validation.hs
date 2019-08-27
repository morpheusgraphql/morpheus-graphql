module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument
  ) where

import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), Key, RawDataType (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)

validatePartialDocument :: [(Key, RawDataType)] -> Validation [(Key, DataFullType)]
validatePartialDocument _ = pure []
