{-# LANGUAGE  DeriveGeneric , DeriveAnyClass , DeriveDataTypeable  #-}

module Data.GraphqlHS.Schema.GQL__EnumValue
  (GQL__EnumValue(..))
where

import           Data.Text                      (Text)
import           GHC.Generics
import           Data.Aeson                     ( ToJSON(..) )
import           Data.Data                      ( Data )

data  GQL__EnumValue = GQL__EnumValue{
  name:: Text
  ,description::Text
  ,isDeprecated:: Bool
  ,deprecationReason:: Text
} deriving (Show , Data, Generic, ToJSON )