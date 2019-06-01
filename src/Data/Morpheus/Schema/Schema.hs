{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Schema.Schema where

import           Data.Morpheus.Kind                                (KIND, OBJECT)
import           Data.Morpheus.Schema.Directive                    (Directive)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (Type, createObjectType, typeFromInputObject,
                                                                    typeFromLeaf, typeFromObject, typeFromUnion)
import           Data.Morpheus.Types.Internal.Data                 (DataOutputObject, DataTypeLib (..))
import           Data.Text                                         (Text)
import           GHC.Generics                                      (Generic)

type instance KIND Schema = OBJECT

data Schema = Schema
  { types            :: [Type]
  , queryType        :: Type
  , mutationType     :: Maybe Type
  , subscriptionType :: Maybe Type
  , directives       :: [Directive]
  } deriving (Generic)

convertTypes :: DataTypeLib -> [Type]
convertTypes lib' =
  [typeFromObject $ query lib'] ++
  typeFromMaybe (mutation lib') ++
  typeFromMaybe (subscription lib') ++
  map typeFromObject (object lib') ++
  map typeFromInputObject (inputObject lib') ++ map typeFromLeaf (leaf lib') ++ map typeFromUnion (union lib')

typeFromMaybe :: Maybe (Text, DataOutputObject) -> [Type]
typeFromMaybe (Just x) = [typeFromObject x]
typeFromMaybe Nothing  = []

buildSchemaLinkType :: (Text, DataOutputObject) -> Type
buildSchemaLinkType (key', _) = createObjectType key' "Query Description" []

initSchema :: DataTypeLib -> Schema
initSchema types' =
  Schema
    { types = convertTypes types'
    , queryType = buildSchemaLinkType $ query types'
    , mutationType = buildSchemaLinkType <$> mutation types'
    , subscriptionType = buildSchemaLinkType <$> subscription types'
    , directives = []
    }
