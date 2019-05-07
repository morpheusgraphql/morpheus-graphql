module Lib
  ( getGQLBody
  , getResponseBody
  , getInfo
  ) where

import           Data.Aeson                 (Value (..), decode)
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

getInfo :: Text -> IO String
getInfo p = readFile (path p ++ "/info.md")

getResponseBody :: Text -> IO Value
getResponseBody p = fromMaybe Null . decode <$> L.readFile (resLib p)
