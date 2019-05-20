{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Schema.Directive
  ( Directive(..)
  ) where

import           Data.Morpheus.Schema.DirectiveLocation            (DirectiveLocation)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (InputValue)
import           Data.Text                                         (Text)
import           GHC.Generics                                      (Generic)

data Directive = Directive
  { name        :: Text
  , description :: Maybe Text
  , locations   :: [DirectiveLocation]
  , args        :: [InputValue]
  } deriving (Generic)

