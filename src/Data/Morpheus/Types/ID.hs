{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Types.ID
  ( ID(..)
  )
where

import qualified Data.Aeson                    as A
import           Data.Morpheus.Kind             ( SCALAR )
import           Data.Morpheus.Types.GQLScalar  ( GQLScalar(..) )
import           Data.Morpheus.Types.GQLType    ( GQLType(KIND) )
import           Data.Morpheus.Types.Internal.AST
                                                ( ScalarValue(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )

-- | default GraphQL type,
-- parses only 'String' and 'Int' values,
-- serialized always as 'String'
newtype ID = ID
  { unpackID :: Text
  } deriving (Show, Generic)

instance GQLType ID where
  type KIND ID = SCALAR

instance A.ToJSON ID where
  toJSON = A.toJSON . unpackID

instance A.FromJSON ID where
  parseJSON = fmap ID . A.parseJSON 

instance GQLScalar ID where
  parseValue (Int    x) = return (ID $ pack $ show x)
  parseValue (String x) = return (ID x)
  parseValue _          = Left ""
  serialize (ID x) = String x
