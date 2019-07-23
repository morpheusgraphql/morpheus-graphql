{-# LANGUAGE OverloadedStrings #-}

module Language.Morpheus
  ( toGraphQLDocument
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus.Types        (GQLRootResolver)

toGraphQLDocument :: GQLRootResolver IO a query mut sub -> IO ByteString
toGraphQLDocument _ = return "Test"
