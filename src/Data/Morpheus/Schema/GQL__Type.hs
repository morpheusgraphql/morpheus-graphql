{-# LANGUAGE DeriveDataTypeable , DeriveGeneric , TypeOperators #-}

module Data.Morpheus.Schema.GQL__Type (GQL__Type(..),GQL__Deprecation__Args) where

import           Data.Text                      (Text)
import           Data.Data                      (Data)
import           GHC.Generics                   (Generic)
import qualified  Data.Morpheus.Schema.GQL__InputValue as I (GQL__InputValue(..))
import qualified  Data.Morpheus.Schema.GQL__Field as  F (GQL__Field(..))
import  Data.Morpheus.Schema.GQL__EnumValue (GQL__EnumValue)
import  Data.Morpheus.Schema.GQL__TypeKind (GQL__TypeKind)
import  Data.Morpheus.Types.Types (EnumOf, (::->)(..))


data GQL__Type =  GQL__Type {
   kind :: EnumOf GQL__TypeKind
  ,name :: Text
  ,description :: Text
  ,fields :: GQL__Deprecation__Args ::-> [F.GQL__Field GQL__Type]
  ,ofType :: Maybe GQL__Type
  ,interfaces :: [GQL__Type]
  ,possibleTypes :: [GQL__Type]
  ,enumValues:: GQL__Deprecation__Args ::-> [GQL__EnumValue]
  ,inputFields:: [I.GQL__InputValue GQL__Type]
} deriving (Show , Data, Generic)

data GQL__Deprecation__Args = DeprecationArgs {
  includeDeprecated:: Maybe Bool
} deriving (Show , Data, Generic )