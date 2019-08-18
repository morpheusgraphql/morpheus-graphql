module GQLClient
  ( gql
  , gqlSchema
  ) where

import qualified Data.ByteString.Lazy      as L (readFile)
import           Data.Morpheus.Client      (parseGQLWith, schemaByDocument)
import           Language.Haskell.TH.Quote

gql :: QuasiQuoter
gql = parseGQLWith $ schemaByDocument (L.readFile "./assets/simple.gql")

gqlSchema = schemaByDocument (L.readFile "./assets/simple.gql")
