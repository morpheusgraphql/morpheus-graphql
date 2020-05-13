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
import Data.Morpheus.Types.Internal.AST (Name (..))
import Data.Text (Text, unpack)

readSource :: Name -> IO ByteString
readSource = L.readFile . path . readName

path :: Text -> String
path name = "test/" ++ unpack name

gqlLib :: Text -> String
gqlLib x = path x ++ "/query.gql"

resLib :: Text -> String
resLib x = path x ++ "/response.json"

maybeVariables :: Name -> IO (Maybe Value)
maybeVariables (Name x) = decode <$> (L.readFile (path x ++ "/variables.json") <|> return "{}")

getGQLBody :: Name -> IO ByteString
getGQLBody (Name p) = L.readFile (gqlLib p)

getCases :: FromJSON a => String -> IO [a]
getCases dir = fromMaybe [] . decode <$> L.readFile ("test/" ++ dir ++ "/cases.json")

getResponseBody :: Name -> IO Value
getResponseBody (Name p) = fromMaybe Null . decode <$> L.readFile (resLib p)
