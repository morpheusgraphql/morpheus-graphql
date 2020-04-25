{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Types
  ( Case(..)
  , Name
  , testWith
  ) where

import qualified Data.Text.Lazy             as LT (toStrict)
import           Data.Text.Lazy.Encoding    (decodeUtf8)

import           Data.Aeson                 (FromJSON, Value, decode, encode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import           Data.Morpheus.Types        (GQLRequest (..), GQLResponse (..))
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text, unpack)
import qualified Data.Text                  as T (concat)
import           GHC.Generics
import           Lib                        (getCases, getGQLBody, getResponseBody, maybeVariables)
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertFailure, testCase)

type Name = Text

data Case = Case
  { path        :: Text
  , description :: String
  } deriving (Generic, FromJSON)

testWith :: (Case -> IO TestTree) -> Name -> IO TestTree
testWith f dir = do
  cases <- getCases (unpack dir)
  test <- sequence $ f <$> map (\x -> x {path = T.concat [dir, "/", path x]}) cases
  return $ testGroup (unpack dir) test