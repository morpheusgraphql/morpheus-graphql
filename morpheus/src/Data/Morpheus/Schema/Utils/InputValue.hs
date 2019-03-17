{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Schema.Utils.InputValue
  ( typeName
  ) where

import qualified Data.Morpheus.Schema.InputValue  as I (InputValue (..))
import qualified Data.Morpheus.Schema.Type        as T (Type (..))
import           Data.Morpheus.Schema.Utils.Utils (InputValue)
import           Data.Text                        (Text)

typeName :: InputValue -> Text
typeName x =
  case I._type x of
    Nothing -> "Error"
    Just t  -> T.name t
