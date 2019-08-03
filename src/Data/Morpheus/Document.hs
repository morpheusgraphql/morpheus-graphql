{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Data.Morpheus.Document
  ( toGraphQLDocument
  , toMorpheusHaskellAPi
  ) where

import           Data.ByteString.Lazy.Char8           (ByteString, pack)

-- MORPHEUS
import           Data.Morpheus.Document.ParseDocument (parseGraphQLDocument)
import           Data.Morpheus.Document.RenderGraphQL (renderGraphQLDocument)
import           Data.Morpheus.Document.RenderHaskell (renderHaskellDocument)
import           Data.Morpheus.Resolve.Resolve        (RootResCon, fullSchema)
import           Data.Morpheus.Types                  (GQLRootResolver)

-- | Generates schema.gql file from 'GQLRootResolver'
toGraphQLDocument :: RootResCon m e c query mut sub => GQLRootResolver m e c query mut sub -> ByteString
toGraphQLDocument x =
  case fullSchema x of
    Left errors -> pack (show errors)
    Right lib   -> renderGraphQLDocument lib

toMorpheusHaskellAPi :: String -> ByteString -> Either ByteString ByteString
toMorpheusHaskellAPi moduleName doc =
  case parseGraphQLDocument doc of
    Left errors -> Left $ pack (show errors)
    Right lib   -> Right $ renderHaskellDocument moduleName lib
