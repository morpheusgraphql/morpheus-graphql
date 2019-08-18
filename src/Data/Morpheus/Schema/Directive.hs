{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Directive
  ( Directive(..)
  ) where

import           Data.Morpheus.Kind                                (OBJECT)
import           Data.Morpheus.Schema.DirectiveLocation            (DirectiveLocation)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (InputValue)
import           Data.Morpheus.Types.GQLType                       (GQLType (KIND, __typeName, __typeVisibility))
import           Data.Text                                         (Text)
import           GHC.Generics                                      (Generic)

instance GQLType Directive where
  type KIND Directive = OBJECT
  __typeName = const "__Directive"
  __typeVisibility = const False

data Directive =
  Directive
    { name        :: Text
    , description :: Maybe Text
    , locations   :: [DirectiveLocation]
    , args        :: [InputValue]
    }
  deriving (Generic)
