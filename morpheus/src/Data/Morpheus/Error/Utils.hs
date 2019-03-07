{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Utils (errorMessage) where

import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Text                      (Text)
import           Data.Morpheus.Types.Error      ( GQLError(..),ErrorLocation(..))

errorMessage :: Text -> [GQLError]
errorMessage x = [GQLError { message = x, locations = [ErrorLocation 0 0] }]
