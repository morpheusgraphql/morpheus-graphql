{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Types.ID
  ( ID(..)
  ) where

import           Data.Morpheus.Kind                 (KIND, SCALAR)
import           Data.Morpheus.Types.GQLScalar      (GQLScalar (..))
import           Data.Morpheus.Types.GQLType        (GQLType)
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..))
import           Data.Text                          (Text, pack)
import           GHC.Generics                       (Generic)

type instance KIND ID = SCALAR

-- | default GraphQL type: ID ,
-- parses only 'String' and 'Int' values,
-- serialized always as 'String'
newtype ID = ID
  { unpackID :: Text
  } deriving (Generic, GQLType)

instance GQLScalar ID where
  parseValue (Int x)    = return (ID $ pack $ show x)
  parseValue (String x) = return (ID x)
  parseValue _          = Left ""
  serialize (ID x) = String x
