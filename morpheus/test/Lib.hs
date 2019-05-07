module Lib
  ( getGQLBody
  , getResponseBody
  ) where

import           Data.Text    (Text, unpack)
import qualified Data.Text.IO as TIO (readFile)

path :: String -> String
path name = "test/lib/" ++ name

gqlLib :: String -> String
gqlLib x = path x ++ ".gql"

resLib :: String -> String
resLib x = path x ++ ".res.json"

getGQLBody :: Text -> IO Text
getGQLBody p = TIO.readFile (gqlLib $ unpack p)

getResponseBody :: Text -> IO Text
getResponseBody p = TIO.readFile (resLib $ unpack p)
