module Lib
  ( getGQLBody
  , getResponseBody
  ) where

import           Data.Aeson                 (Value (..), decode)
import qualified Data.ByteString.Lazy       as L (readFile)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text, unpack)

path :: String -> String
path name = "test/lib/" ++ name

gqlLib :: String -> String
gqlLib x = path x ++ "/query.gql"

resLib :: String -> String
resLib x = path x ++ "/response.json"

getGQLBody :: Text -> IO ByteString
getGQLBody p = L.readFile (gqlLib $ unpack p)

getResponseBody :: Text -> IO Value
getResponseBody p = fromMaybe Null . decode <$> L.readFile (resLib $ unpack p)
