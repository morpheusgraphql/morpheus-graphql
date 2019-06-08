{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Schema
  ( nameCollisionError
  ) where

import           Data.Text (Text)

nameCollisionError :: Text -> Text
nameCollisionError name = "Name collision: \"" <> name <> "\" is used for different dataTypes in two separate modules"
