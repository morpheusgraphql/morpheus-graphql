module GQLSchema
  ( gqlSchema
  ) where

import qualified Data.ByteString.Lazy as L (readFile)
import           Data.Morpheus.Client (schemaByDocument)

gqlSchema = schemaByDocument (L.readFile "./assets/simple.gql")
