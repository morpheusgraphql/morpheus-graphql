{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Directive
  ( Directive(..)
  ) where

import           Data.Morpheus.Kind                                (KIND, OBJECT)
import           Data.Morpheus.Schema.DirectiveLocation            (DirectiveLocation)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (InputValue)
import           Data.Morpheus.Types.GQLType                       (GQLType (__typeName, __typeVisibility))
import           Data.Text                                         (Text)
import           GHC.Generics                                      (Generic)

type instance KIND Directive = OBJECT

instance GQLType Directive where
  __typeName = const "__Directive"
  __typeVisibility = const False

data Directive = Directive
  { name        :: Text
  , description :: Maybe Text
  , locations   :: [DirectiveLocation]
  , args        :: [InputValue]
  } deriving (Generic)
