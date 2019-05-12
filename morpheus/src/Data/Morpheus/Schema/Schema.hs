{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Schema.Schema where

import           Data.Morpheus.Schema.Directive ( Directive )
import           Data.Morpheus.Schema.Internal.AST
                                                ( OutputObject
                                                , TypeLib(..)
                                                )
import           Data.Morpheus.Schema.Utils.Utils
                                                ( Type
                                                , createObjectType
                                                , typeFromInputObject
                                                , typeFromLeaf
                                                , typeFromObject
                                                , typeFromUnion
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data Schema = Schema
  { types            :: [Type]
  , queryType        :: Type
  , mutationType     :: Maybe Type
  , subscriptionType :: Maybe Type
  , directives       :: [Directive]
  } deriving (Generic)

convertTypes :: TypeLib -> [Type]
convertTypes lib' =
  [typeFromObject $ query lib']
    ++ typeFromMaybe (mutation lib')
    ++ typeFromMaybe (subscription lib')
    ++ map typeFromObject      (object lib')
    ++ map typeFromInputObject (inputObject lib')
    ++ map typeFromLeaf        (leaf lib')
    ++ map typeFromUnion       (union lib')

typeFromMaybe :: Maybe (Text, OutputObject) -> [Type]
typeFromMaybe (Just x) = [typeFromObject x]
typeFromMaybe Nothing  = []

buildSchemaLinkType :: (Text, OutputObject) -> Type
buildSchemaLinkType (key', _) = createObjectType key' "Query Description" []

initSchema :: TypeLib -> Schema
initSchema types' = Schema
  { types            = convertTypes types'
  , queryType        = buildSchemaLinkType $ query types'
  , mutationType     = buildSchemaLinkType <$> mutation types'
  , subscriptionType = buildSchemaLinkType <$> subscription types'
  , directives       = []
  }
