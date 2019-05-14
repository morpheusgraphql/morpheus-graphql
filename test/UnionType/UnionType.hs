{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module UnionType.UnionType
  ( api
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (GQLQuery, GQLType (..), KIND, OBJECT, UNION)
import           Data.Morpheus.Types        ((::->) (..), GQLRoot (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

type instance KIND A = OBJECT

type instance KIND B = OBJECT

type instance KIND AOrB = UNION

data A = A
  { aText :: Text
  , aInt  :: Int
  } deriving (Generic, GQLType)

data B = B
  { bText :: Text
  , bInt  :: Int
  } deriving (Generic, GQLType)

data AOrB
  = A' A
  | B' B
  deriving (Generic, GQLType)

newtype Query = Query
  { union :: () ::-> [AOrB]
  } deriving (Generic, GQLQuery)

resolveUnion :: () ::-> [AOrB]
resolveUnion = return [A' A {aText = "at", aInt = 1}, B' B {bText = "bt", bInt = 2}]

api :: ByteString -> IO ByteString
api = interpreter GQLRoot {query = Query {union = resolveUnion}, mutation = (), subscription = ()}
