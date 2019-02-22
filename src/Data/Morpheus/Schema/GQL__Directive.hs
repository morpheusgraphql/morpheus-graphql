{-# LANGUAGE DeriveGeneric, DeriveAnyClass , DeriveDataTypeable , TypeOperators  #-}

module Data.Morpheus.Schema.GQL__Directive
  (GQL__Directive(..))
where

import           Data.Text                      ( Text(..)
                                                , pack
                                                )
import           Data.Map                       ( Map
                                                , fromList
                                                )
import           GHC.Generics
import           Data.Aeson                     ( ToJSON(..) )
import           Data.Data                      ( Data )
import           Data.Morpheus.Types.Types     ( (::->)(..) )
import           Data.List                      ( find )
import           Data.Morpheus.Schema.GQL__DirectiveLocation  (GQL__DirectiveLocation)
import           Data.Morpheus.Types.Introspection ( GQL__InputValue)

data  GQL__Directive  = GQL__Directive {
  name :: Text
  ,description :: Text
  ,locations:: [GQL__DirectiveLocation]
  ,args:: [GQL__InputValue]
} deriving (Show , Data, Generic )