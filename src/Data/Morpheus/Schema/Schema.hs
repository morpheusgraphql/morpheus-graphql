{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
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

import           Data.Text                                         (Text)
import           GHC.Generics                                      (Generic)

-- MORPHEUS
import           Data.Morpheus.Kind                                (OBJECT)
import           Data.Morpheus.Schema.Directive                    (Directive)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (Type, createObjectType, renderType)
import           Data.Morpheus.Types.GQLType                       (GQLType (KIND, __typeName, __typeVisibility))
import           Data.Morpheus.Types.Internal.Data                 (DataOutputObject, DataTypeLib (..), allDataTypes)
import           Data.Morpheus.Types.Internal.Validation           (Validation)

instance GQLType Schema where
  type KIND Schema = OBJECT
  __typeName = const "__Schema"
  __typeVisibility = const False

data Schema = Schema
  { types            :: [Type]
  , queryType        :: Type
  , mutationType     :: Maybe Type
  , subscriptionType :: Maybe Type
  , directives       :: [Directive Type]
  } deriving (Generic)

convertTypes :: DataTypeLib -> Validation [Type]
convertTypes lib = traverse (`renderType` lib) (allDataTypes lib)

buildSchemaLinkType :: (Text, DataOutputObject) -> Type
buildSchemaLinkType (key', _) = createObjectType key' "" $ Just []

findType :: Text -> DataTypeLib -> Maybe Type
findType name lib = (name, ) <$> lookup name (allDataTypes lib) >>= renderT
  where
    renderT i =
      case renderType i lib of
        Left _  -> Nothing
        Right x -> Just x

initSchema :: DataTypeLib -> Validation Schema
initSchema lib = do
  types <- convertTypes lib
  pure $
    Schema
      { types
      , queryType = buildSchemaLinkType $ query lib
      , mutationType = buildSchemaLinkType <$> mutation lib
      , subscriptionType = buildSchemaLinkType <$> subscription lib
      , directives = []
      }
