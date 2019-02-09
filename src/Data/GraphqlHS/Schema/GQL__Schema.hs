{-# LANGUAGE DeriveGeneric ,DeriveAnyClass , DeriveDataTypeable , OverloadedStrings #-}

module Data.GraphqlHS.Schema.GQL__Schema where

import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                , omitNothingFields
                                                , genericToJSON
                                                , defaultOptions
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.GraphqlHS.Generics.GQLArgs
                                                ( GQLArgs )
import           Data.GraphqlHS.Types.Types     ( (::->)(..)
                                                , Eval(..)
                                                )
import           Data.GraphqlHS.Types.Introspection
                                                ( GQL__Type
                                                , emptyLib
                                                )
import           Data.GraphqlHS.Schema.GQL__Directive
                                                ( GQL__Directive
                                                )

import           Data.Proxy                     ( Proxy(..) )

data GQL__Schema = GQL__Schema {
     types::[GQL__Type]
    , queryType:: Maybe GQL__Type
    , mutationType::Maybe GQL__Type
    , subscriptionType:: Maybe GQL__Type
    , directives:: [GQL__Directive]
} deriving (Show , Data, Generic, ToJSON )

initSchema :: [GQL__Type] -> GQL__Schema
initSchema types = GQL__Schema
    { types            = types
    , queryType        = Nothing
    , mutationType     = Nothing
    , subscriptionType = Nothing
    , directives       =  []
    }