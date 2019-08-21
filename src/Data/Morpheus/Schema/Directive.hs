{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Directive
  ( Directive(..)
  ) where

import           Data.Aeson                             (FromJSON (..))
import           Data.Morpheus.Kind                     (OBJECT)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.InputValue        (InputValue)
import           Data.Morpheus.Types.GQLType            (GQLType (KIND, __typeName, __typeVisibility))
import           Data.Text                              (Text)
import           Data.Typeable                          (Typeable)
import           GHC.Generics                           (Generic)

instance Typeable a => GQLType (Directive a) where
  type KIND (Directive a) = OBJECT
  __typeName = const "__Directive"
  __typeVisibility = const False

data Directive t = Directive
  { name        :: Text
  , description :: Maybe Text
  , locations   :: [DirectiveLocation]
  , args        :: [InputValue t]
  } deriving (Show, Generic, FromJSON)
