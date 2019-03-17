{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Schema.Schema where

import           Data.Data                        (Data)
import qualified Data.Map                         as M
import           Data.Morpheus.Schema.Directive   (Directive)
import           Data.Morpheus.Schema.Utils.Utils (Type, createType)
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)

data Schema = Schema
  { types            :: [Type]
  , queryType        :: Maybe Type
  , mutationType     :: Maybe Type
  , subscriptionType :: Maybe Type
  , directives       :: [Directive]
  } deriving (Show, Data, Generic)

initSchema :: M.Map Text Type -> Schema
initSchema sTypes =
  Schema
    { types = M.elems sTypes
    , queryType = Just $ createType "Query" []
    , mutationType = Nothing
    , subscriptionType = Nothing
    , directives = []
    }
