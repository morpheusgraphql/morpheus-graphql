{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Execution.Internal.Decode
  ( withObject
  , decodeFieldWith
  ) where

import           Data.Semigroup                          ((<>))

-- MORPHEUS
import           Data.Morpheus.Error.Internal            (internalArgumentError, internalTypeMismatch)
import           Data.Morpheus.Types.Internal.Data       (Key)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Types.Internal.Value      (Object, Value (..))

withObject :: (Object -> Validation a) -> Value -> Validation a
withObject f (Object object) = f object
withObject _ isType          = internalTypeMismatch "Object" isType

decodeFieldWith :: (Value -> Validation a) -> Key -> Object -> Validation a
decodeFieldWith decoder name object =
  case lookup name object of
    Nothing    -> internalArgumentError ("Missing Field: \"" <> name <> "\"")
    Just value -> decoder value
