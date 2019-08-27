{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Data.Morpheus.Document
  ( toGraphQLDocument
  , toMorpheusHaskellAPi
  , gqlDocument
  , parseFullGQLDocument
  ) where

import           Control.Monad                                ((>=>))
import           Data.ByteString.Lazy.Char8                   (ByteString, pack)
import           Data.Text                                    (Text)
import qualified Data.Text.Lazy                               as LT (toStrict)
import           Data.Text.Lazy.Encoding                      (decodeUtf8)
import           Language.Haskell.TH.Quote

--
-- MORPHEUS
import           Data.Morpheus.Execution.Document.Compile     (compileDec, compileExp)
import           Data.Morpheus.Execution.Server.Resolve       (RootResCon, fullSchema)
import           Data.Morpheus.Rendering.GQL                  (renderGraphQLDocument)
import           Data.Morpheus.Rendering.Haskell.Render       (renderHaskellDocument)
import           Data.Morpheus.Types                          (GQLRootResolver)

import           Data.Morpheus.Parsing.Document.Parser        (parseTypes)
import           Data.Morpheus.Parsing.Internal.Create        (createDataTypeLib)
import           Data.Morpheus.Schema.SchemaAPI               (defaultTypes)
import           Data.Morpheus.Types.Internal.Data            (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation      (Validation)
import           Data.Morpheus.Validation.Document.Validation (validatePartialDocument)

parseDocument :: Text -> Validation DataTypeLib
parseDocument doc = parseTypes doc >>= validatePartialDocument >>= createDataTypeLib

parseGraphQLDocument :: ByteString -> Validation DataTypeLib
parseGraphQLDocument x = parseDocument (LT.toStrict $ decodeUtf8 x)

parseFullGQLDocument :: ByteString -> Validation DataTypeLib
parseFullGQLDocument = parseGraphQLDocument >=> defaultTypes

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

gqlDocument :: QuasiQuoter
gqlDocument =
  QuasiQuoter
    {quoteExp = compileExp, quotePat = notHandled "Patterns", quoteType = notHandled "Types", quoteDec = compileDec}
  where
    notHandled things = error $ things ++ " are not supported by the GraphQL QuasiQuoter"
