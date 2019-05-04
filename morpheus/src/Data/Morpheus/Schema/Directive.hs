{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Morpheus.Schema.Directive
  ( Directive(..)
  ) where

import           Data.Data                              (Data)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.Utils.Utils       (InputValue)
import           Data.Text                              (Text)
import           GHC.Generics

data Directive = Directive
  { name        :: Text
  , description :: Maybe Text
  , locations   :: [DirectiveLocation]
  , args        :: [InputValue]
  } deriving (Show, Data, Generic)
