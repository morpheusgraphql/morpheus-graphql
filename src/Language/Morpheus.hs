{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Morpheus
  ( toGraphQLDocument
  ) where

import           Data.ByteString.Lazy.Char8        (ByteString, pack)
import           Data.Morpheus.Resolve.Resolve     (RootResCon, fullSchema)
import           Data.Morpheus.Types               (GQLRootResolver)
import           Data.Morpheus.Types.Internal.Data (DataFullType, DataOutputObject, DataTypeLib (..), allDataTypes)
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
renderType (x, _) = "type " <> x <> " { \n  \n}"
