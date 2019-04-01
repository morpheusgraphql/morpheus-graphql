{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Schema.Schema where

import           Data.Data                           (Data)
import           Data.Morpheus.Schema.Directive      (Directive)
import           Data.Morpheus.Schema.Internal.Types (OutputObject, TypeLib (..))
import           Data.Morpheus.Schema.Utils.Utils    (Type, createObjectType, typeFromInputObject, typeFromLeaf,
                                                      typeFromObject)
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)

data Schema = Schema
  { types            :: [Type]
  , queryType        :: Type
  , mutationType     :: Maybe Type
  , subscriptionType :: Maybe Type
  , directives       :: [Directive]
  } deriving (Show, Data, Generic)

convertTypes :: TypeLib -> [Type]
convertTypes lib' =
  [typeFromObject $ query lib'] ++
  typeFromMutation (mutation lib') ++
  map typeFromObject (object lib') ++ map typeFromInputObject (inputObject lib') ++ map typeFromLeaf (leaf lib')

typeFromMutation :: Maybe (Text, OutputObject) -> [Type]
typeFromMutation (Just x) = [typeFromObject x]
typeFromMutation Nothing  = []

initSchema :: TypeLib -> Schema
initSchema types' =
  Schema
    { types = convertTypes types'
    , queryType = createObjectType (fst $ query types') "Query Description" []
    , mutationType = Nothing
    , subscriptionType = Nothing
    , directives = []
    }
