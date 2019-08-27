{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Document.Interface
  ( unknownInterface
  ) where

import           Data.Morpheus.Error.Utils               (errorMessage)
import           Data.Morpheus.Types.Internal.Base       (Key, Position)
import           Data.Morpheus.Types.Internal.Validation (GQLErrors)
import           Data.Semigroup                          ((<>))

unknownInterface :: Key -> Position -> GQLErrors
unknownInterface name = (`errorMessage` message)
  where
    message = "Unknown Interface \"" <> name <> "\"."
