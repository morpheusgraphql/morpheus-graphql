module Lib
  ( getGQLBody
  , getResponseBody
  ) where

import           Data.Text    (Text, unpack)
import qualified Data.Text.IO as TIO (readFile)

path :: String -> String
path name = "test/" ++ name ++ ".gql"

gqlLib :: String -> String
gqlLib x = path x ++ ".gql"

getGQLBody :: Text -> IO Text
getGQLBody p = TIO.readFile (gqlLib $ unpack p)

getResponseBody :: Text -> IO Text
getResponseBody p = TIO.readFile (gqlLib $ unpack p)
