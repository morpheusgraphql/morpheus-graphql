{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Data.Morpheus.Document
  ( toGraphQLDocument
  ) where

import           Data.ByteString.Lazy.Char8           (ByteString, pack)
import           Data.Morpheus.Document.ParseDocument (parseGraphQLDocument)

--import           Data.Morpheus.Document.RenderGraphQL (renderGraphQLDocument)
-- MORPHEUS
import           Data.Morpheus.Resolve.Resolve        (RootResCon, fullSchema)
import           Data.Morpheus.Types                  (GQLRootResolver)

-- | Generates schema.gql file from 'GQLRootResolver'
toGraphQLDocument :: RootResCon m a query mut sub => GQLRootResolver m a query mut sub -> ByteString
toGraphQLDocument x =
  case fullSchema x of
    Left validationError -> pack (show validationError)
    Right lib            -> parseGraphQLDocument
    --renderGraphQLDocument lib
