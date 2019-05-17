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
import           Data.Morpheus.Kind         (GQLArgs, GQLQuery, GQLType (..), INPUT_OBJECT, KIND, OBJECT)
import           Data.Morpheus.Types        ((::->) (..), GQLRoot (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

type instance KIND A = OBJECT

type instance KIND B = INPUT_OBJECT

data A = A
  { aText :: Text
  , aInt  :: Int
  } deriving (Generic, GQLType)

data B = B
  { bText :: Text
  , bInt  :: Int
  } deriving (Generic, GQLType)

data F1Args = F1Args
  { arg1 :: Text
  , arg2 :: Maybe B
  } deriving (Generic, GQLArgs)

newtype Query = Query
  { f1 :: F1Args ::-> A
  } deriving (Generic, GQLQuery)

resolveUnion :: F1Args ::-> A
resolveUnion = return $ A {aText = "at", aInt = 1}

api :: ByteString -> IO ByteString
api = interpreter GQLRoot {query = Query {f1 = resolveUnion}, mutation = (), subscription = ()}
