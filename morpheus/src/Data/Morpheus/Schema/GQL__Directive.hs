{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Morpheus.Schema.GQL__Directive
  ( GQL__Directive(..)
  ) where

import           Data.Data                                   (Data)
import           Data.Morpheus.Schema.GQL__DirectiveLocation (GQL__DirectiveLocation)
import           Data.Morpheus.Types.Introspection           (GQL__InputValue)
import           Data.Morpheus.Types.Types                   (EnumOf (..))
import           Data.Text                                   (Text)
import           GHC.Generics

data GQL__Directive = GQL__Directive
  { name        :: Text
  , description :: Text
  , locations   :: [EnumOf GQL__DirectiveLocation]
  , args        :: [GQL__InputValue]
  } deriving (Show, Data, Generic)
