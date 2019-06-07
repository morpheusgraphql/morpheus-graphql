{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.InputType.API
  ( api
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (KIND, OBJECT)
import           Data.Morpheus.Types        ((::->), GQLArgs, GQLQuery, GQLRoot (..), GQLType (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

type instance KIND A = OBJECT

data F1Args = F1Args
  { arg1 :: Text
  , arg2 :: Maybe Int
  } deriving (Generic, GQLArgs)

data F2Args = F2Args
  { argList       :: [Text]
  , argNestedList :: [Maybe [[Int]]]
  } deriving (Generic, GQLArgs)

data A = A
  { a1 :: F1Args ::-> Text
  , a2 :: F2Args ::-> Int
  } deriving (Generic, GQLType)

newtype Query = Query
  { q1 :: A
  } deriving (Generic, GQLQuery)

api :: ByteString -> IO ByteString
api =
  interpreter
    GQLRoot
      { queryResolver = Query {q1 = A {a1 = return "a1Test", a2 = return 1}}
      , mutationResolver = ()
      , subscriptionResolver = ()
      }
