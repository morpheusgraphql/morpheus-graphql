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
import Data.Morpheus.Types.Internal.AST (FieldName (..))
import Data.Text (Text, unpack)

readSource :: FieldName -> IO ByteString
readSource = L.readFile . path . readName

path :: Text -> String
path name = "test/" ++ unpack name

gqlLib :: Text -> String
gqlLib x = path x ++ "/query.gql"

resLib :: Text -> String
resLib x = path x ++ "/response.json"

maybeVariables :: FieldName -> IO (Maybe Value)
maybeVariables (FieldName x) = decode <$> (L.readFile (path x ++ "/variables.json") <|> return "{}")

getGQLBody :: FieldName -> IO ByteString
getGQLBody (FieldName p) = L.readFile (gqlLib p)

getCases :: FromJSON a => String -> IO [a]
getCases dir = fromMaybe [] . decode <$> L.readFile ("test/" ++ dir ++ "/cases.json")

getResponseBody :: FieldName -> IO Value
getResponseBody (FieldName p) = fromMaybe Null . decode <$> L.readFile (resLib p)
