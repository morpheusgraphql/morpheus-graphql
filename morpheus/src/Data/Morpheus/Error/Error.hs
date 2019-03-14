{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Error
  ( handleError
  ) where

import           Data.Morpheus.Error.Utils (errorMessage)
import           Data.Morpheus.Types.Error (GQLErrors)
import           Data.Text                 (Text)
import qualified Data.Text                 as T (concat)

-- GQL:: if no mutation defined -> "Schema is not configured for mutations."
handleError :: Text -> Either GQLErrors b
handleError x = Left $ errorMessage 0 $ T.concat ["Field Error: ", x]
