module GQLClient
  ( gql
  ) where

import qualified Data.ByteString.Lazy      as L (readFile)
import           Data.Morpheus.Client      (parseGQLWith, schemaByDocument)
import           Language.Haskell.TH.Quote

gql :: QuasiQuoter
gql = parseGQLWith $ schemaByDocument (L.readFile "./assets/simple.gql")
