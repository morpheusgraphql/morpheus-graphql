{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getGQLBody,
    getResponseBody,
    getCases,
    maybeVariables,
    readSource,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, Value (..), decode)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)

readSource :: Text -> IO ByteString
readSource p = L.readFile (path p <> "schema.gql")

path :: Text -> String
path name = "test/" ++ unpack name

gqlLib :: Text -> String
gqlLib x = path x ++ "/query.gql"

resLib :: Text -> String
resLib x = path x ++ "/response.json"

maybeVariables :: Text -> IO (Maybe Value)
maybeVariables x = decode <$> (L.readFile (path x ++ "/variables.json") <|> return "{}")

getGQLBody :: Text -> IO ByteString
getGQLBody p = L.readFile (gqlLib p)

getCases :: FromJSON a => String -> IO [a]
getCases dir = fromMaybe [] . decode <$> L.readFile ("test/" ++ dir ++ "/cases.json")

getResponseBody :: Text -> IO Value
getResponseBody p = fromMaybe Null . decode <$> L.readFile (resLib p)
