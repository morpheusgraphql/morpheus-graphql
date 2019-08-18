{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Data.Morpheus.Document
  ( toGraphQLDocument
  , toMorpheusHaskellAPi
  ) where

import           Data.ByteString.Lazy.Char8             (ByteString, pack)

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Resolve (RootResCon, fullSchema)
import           Data.Morpheus.Parsing.Document.Parse   (parseGraphQLDocument)
import           Data.Morpheus.Rendering.GQL            (renderGraphQLDocument)
import           Data.Morpheus.Rendering.Haskell.Render (renderHaskellDocument)
import           Data.Morpheus.Types                    (GQLRootResolver)

-- | Generates schema.gql file from 'GQLRootResolver'
toGraphQLDocument :: RootResCon m e c query mut sub => proxy (GQLRootResolver m e c query mut sub) -> ByteString
toGraphQLDocument x =
  case fullSchema x of
    Left errors -> pack (show errors)
    Right lib   -> renderGraphQLDocument lib

toMorpheusHaskellAPi :: String -> ByteString -> Either ByteString ByteString
toMorpheusHaskellAPi moduleName doc =
  case parseGraphQLDocument doc of
    Left errors -> Left $ pack (show errors)
    Right lib   -> Right $ renderHaskellDocument moduleName lib
