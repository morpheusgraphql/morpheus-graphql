{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Client
  ( gql
  , defineQuery
  , Fetch(..)
  ) where

import qualified Data.ByteString.Lazy         as L (readFile)
import           Data.Morpheus.Client.Build   (Fetch (..), defineQuery)
import           Data.Morpheus.Client.Compile (compile)
import           Language.Haskell.TH.Quote

gql :: QuasiQuoter
gql =
  QuasiQuoter
    { quoteExp = compile $ L.readFile "./assets/simple.gql"
    , quotePat = notHandled "patterns"
    , quoteType = notHandled "types"
    , quoteDec = notHandled "declarations"
    }
  where
    notHandled things = error $ things ++ " are not supported by the GraphQL QuasiQuoter"
