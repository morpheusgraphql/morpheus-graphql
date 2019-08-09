{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.Input.Enum.API
  ( api
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Types        (GQLRootResolver (..), GQLType (..), IORes, resolver)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

import           Data.Morpheus.Kind         (ENUM, OBJECT)

-- types & args
newtype TestArgs = TestArgs
  { ttt :: Level
  } deriving (Generic, Show)

instance GQLType TestArgs where
  type KIND TestArgs = ENUM

data Level
  = L0
  | L1
  | L2
  | L3
  deriving (Show, Generic)

instance GQLType Level where
  type KIND Level = ENUM

data TestResult = TestResult
  { code  :: Text
  , level :: Level
  } deriving (Generic, Show)

instance GQLType TestResult where
  type KIND TestResult = OBJECT

-- query
qTest :: TestArgs -> IORes TestResult
qTest args = resolver $ return $ Right $ TestResult {code = "ok", level = ttt args}

-- resolver
newtype Query = Query
  { test :: TestArgs -> IORes TestResult
  } deriving (Generic)

rootResolver :: GQLRootResolver IO () () Query () ()
rootResolver =
  GQLRootResolver
    {queryResolver = return Query {test = qTest}, mutationResolver = return (), subscriptionResolver = return ()}

api :: ByteString -> IO ByteString
api = interpreter rootResolver
