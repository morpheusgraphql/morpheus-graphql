{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Syntax
  ( syntaxError
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLErrors)
import           Data.Morpheus.Types.MetaInfo (Position)
import           Data.Text                    (Text)
import qualified Data.Text                    as T (concat)


syntaxError :: Text -> Position -> GQLErrors
syntaxError e pos = errorMessage pos $ T.concat ["Syntax Error: ", e]
