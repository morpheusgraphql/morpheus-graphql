{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Syntax
  ( syntaxError
  ) where

import           Data.Morpheus.Error.Utils               (errorMessage)
import           Data.Morpheus.Types.Internal.Base       (Position)
import           Data.Morpheus.Types.Internal.Validation (GQLErrors)
import           Data.Text                               (Text)
import qualified Data.Text                               as T (concat)

syntaxError :: Text -> Position -> GQLErrors
syntaxError e pos = errorMessage pos $ T.concat ["Syntax Error: ", e]
