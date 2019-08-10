{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Schema
  ( initSchema
  , findType
  , Schema
  , Type
  ) where

import           Data.Morpheus.Kind                                (OBJECT)
import           Data.Morpheus.Schema.Directive                    (Directive)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (Type, createObjectType, renderType)
import           Data.Morpheus.Types.GQLType                       (GQLType (KIND, __typeName, __typeVisibility))
import           Data.Morpheus.Types.Internal.Data                 (DataOutputObject, DataTypeLib (..), allDataTypes)
import           Data.Text                                         (Text)
import           GHC.Generics                                      (Generic)

instance GQLType Schema where
  type KIND Schema = OBJECT
  __typeName = const "__Schema"
  __typeVisibility = const False

data Schema = Schema
  { types            :: [Type]
  , queryType        :: Type
  , mutationType     :: Maybe Type
  , subscriptionType :: Maybe Type
  , directives       :: [Directive]
  } deriving (Generic)

convertTypes :: DataTypeLib -> [Type]
convertTypes lib' = map renderType (allDataTypes lib')

buildSchemaLinkType :: (Text, DataOutputObject) -> Type
buildSchemaLinkType (key', _) = createObjectType key' "" $ Just []

findType :: Text -> DataTypeLib -> Maybe Type
findType name lib = renderType . (name, ) <$> lookup name (allDataTypes lib)

initSchema :: DataTypeLib -> Schema
initSchema types' =
  Schema
    { types = convertTypes types'
    , queryType = buildSchemaLinkType $ query types'
    , mutationType = buildSchemaLinkType <$> mutation types'
    , subscriptionType = buildSchemaLinkType <$> subscription types'
    , directives = []
    }
