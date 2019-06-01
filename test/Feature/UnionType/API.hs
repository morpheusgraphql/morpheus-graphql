{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.UnionType.API
  ( api
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (KIND, OBJECT, UNION)
import           Data.Morpheus.Types        ((::->), GQLQuery, GQLRoot (..), GQLType (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

type instance KIND A = OBJECT

type instance KIND B = OBJECT

type instance KIND C = OBJECT

type instance KIND AOrB = UNION

data A = A
  { aText :: Text
  , aInt  :: Int
  } deriving (Generic, GQLType)

data B = B
  { bText :: Text
  , bInt  :: Int
  } deriving (Generic, GQLType)

data C = C
  { cText :: Text
  , cInt  :: Int
  } deriving (Generic, GQLType)

data AOrB
  = A' A
  | B' B
  deriving (Generic, GQLType)

data Query = Query
  { union :: () ::-> [AOrB]
  , fc    :: C
  } deriving (Generic, GQLQuery)

resolveUnion :: () ::-> [AOrB]
resolveUnion = return [A' A {aText = "at", aInt = 1}, B' B {bText = "bt", bInt = 2}]

api :: ByteString -> IO ByteString
api =
  interpreter
    GQLRoot {query = Query {union = resolveUnion, fc = C {cText = "", cInt = 3}}, mutation = (), subscription = ()}
