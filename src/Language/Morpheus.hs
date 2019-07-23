{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Morpheus
  ( toGraphQLDocument
  ) where

import           Data.ByteString.Lazy.Char8        (ByteString, pack)
import           Data.Morpheus.Resolve.Resolve     (RootResCon, fullSchema)
import           Data.Morpheus.Types               (GQLRootResolver)
import           Data.Morpheus.Types.Internal.Data (DataFullType (..), DataLeaf (..), DataOutputObject,
                                                    DataTypeLib (..), allDataTypes)
import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text, intercalate)
import qualified Data.Text.Lazy                    as LT (fromStrict)
import           Data.Text.Lazy.Encoding           (encodeUtf8)

toGraphQLDocument :: RootResCon m a query mut sub => GQLRootResolver m a query mut sub -> ByteString
toGraphQLDocument x =
  case fullSchema x of
    Left validationError -> pack (show validationError)
    Right lib            -> encodeUtf8 $ LT.fromStrict $ intercalate "\n\n" $ map renderType $allDataTypes lib

renderType :: (Text, DataFullType) -> Text
renderType (name, Leaf (LeafScalar _)) = "scalar " <> name
renderType (name, Leaf (LeafEnum _))   = "enum " <> name <> " = | "
renderType (name, Union _)             = "union " <> name <> " = | "
renderType (name, InputObject _)       = "inputObject " <> name <> " { \n  \n}"
renderType (name, InputUnion _)        = "inputObject " <> name <> " { \n  \n}"
renderType (name, OutputObject _)      = "type " <> name <> " { \n  \n}"
