{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Morpheus.Schema.Directive
  ( Directive(..)
  ) where

import           Data.Data                              (Data)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.Utils.Utils       (InputValue)
import           Data.Morpheus.Types.Describer          (EnumOf (..))
import           Data.Text                              (Text)
import           GHC.Generics

data Directive = Directive
  { name        :: Text
  , description :: Text
  , locations   :: [EnumOf DirectiveLocation]
  , args        :: [InputValue]
  } deriving (Show, Data, Generic)
