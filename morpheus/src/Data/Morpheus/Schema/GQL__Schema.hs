{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Schema.GQL__Schema where

import           Data.Data                           (Data)
import qualified Data.Map                            as M
import           Data.Morpheus.Schema.GQL__Directive (GQL__Directive)
import           Data.Morpheus.Types.Introspection   (GQL__Type, createType)
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)

data GQL__Schema = GQL__Schema
  { types            :: [GQL__Type]
  , queryType        :: Maybe GQL__Type
  , mutationType     :: Maybe GQL__Type
  , subscriptionType :: Maybe GQL__Type
  , directives       :: [GQL__Directive]
  } deriving (Show, Data, Generic)

initSchema :: M.Map Text GQL__Type -> GQL__Schema
initSchema sTypes =
  GQL__Schema
    { types = M.elems sTypes
    , queryType = Just $ createType "Query" []
    , mutationType = Nothing
    , subscriptionType = Nothing
    , directives = []
    }
