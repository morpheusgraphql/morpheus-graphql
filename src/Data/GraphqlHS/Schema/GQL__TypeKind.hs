{-# LANGUAGE DeriveGeneric, DeriveDataTypeable  ,  DeriveAnyClass #-}

module Data.GraphqlHS.Schema.GQL__TypeKind
  (GQL__TypeKind(..))
where

import           GHC.Generics
import           Data.Aeson                     ( ToJSON(..) )
import           Data.Data                      ( Data )

data  GQL__TypeKind = SCALAR | OBJECT | INTERFACE | UNION | ENUM | INPUT_OBJECT | LIST | NON_NULL deriving (Show , Data, Generic, ToJSON )
