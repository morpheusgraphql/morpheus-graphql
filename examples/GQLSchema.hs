module GQLSchema
  ( gqlSchema
  ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L (readFile)

gqlSchema :: IO ByteString
gqlSchema = L.readFile "./assets/simple.gql"
