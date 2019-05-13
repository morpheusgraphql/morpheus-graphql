{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( getGQLBody
  , getResponseBody
  , getCases
  ) where

import           Data.Aeson                 (FromJSON, Value (..), decode)
import qualified Data.ByteString.Lazy       as L (readFile)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text, unpack)

path :: Text -> String
path name = "test/lib/" ++ unpack name

gqlLib :: Text -> String
gqlLib x = path x ++ "/query.gql"

resLib :: Text -> String
resLib x = path x ++ "/response.json"

getGQLBody :: Text -> IO ByteString
getGQLBody p = L.readFile (gqlLib p)

getCases :: FromJSON a => IO [a]
getCases = fromMaybe [] . decode <$> L.readFile (path "cases.json")

getResponseBody :: Text -> IO Value
getResponseBody p = fromMaybe Null . decode <$> L.readFile (resLib p)
