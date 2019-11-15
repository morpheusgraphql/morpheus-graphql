{-# LANGUAGE FlexibleContexts #-}

module Data.Morpheus.Document
  ( toGraphQLDocument
  , toMorpheusHaskellAPi
  , gqlDocument
  , parseFullGQLDocument
  , importGQLDocument
  , importGQLDocumentWithNamespace
  )
where

import           Control.Monad                  ( (>=>) )
import           Data.ByteString.Lazy.Char8     ( ByteString
                                                , pack
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as LT
                                                ( toStrict )
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Document.Compile
                                                ( compileDocument
                                                , gqlDocument
                                                )
import           Data.Morpheus.Execution.Server.Resolve
                                                ( RootResCon
                                                , fullSchema
                                                )
import           Data.Morpheus.Parsing.Document.Parser
                                                ( parseTypes )
import           Data.Morpheus.Rendering.Haskell.Render
                                                ( renderHaskellDocument )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( renderGraphQLDocument )
import           Data.Morpheus.Schema.SchemaAPI ( defaultTypes )
import           Data.Morpheus.Types            ( GQLRootResolver )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataTypeLib
                                                , createDataTypeLib
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation
                                                , Computation(..)
                                                )
import           Data.Morpheus.Validation.Document.Validation
                                                ( validatePartialDocument )

parseDocument :: Text -> Validation DataTypeLib
parseDocument doc =
  parseTypes doc >>= validatePartialDocument >>= createDataTypeLib

parseGraphQLDocument :: ByteString -> Validation DataTypeLib
parseGraphQLDocument x = parseDocument (LT.toStrict $ decodeUtf8 x)

parseFullGQLDocument :: ByteString -> Validation DataTypeLib
parseFullGQLDocument = parseGraphQLDocument >=> defaultTypes

-- | Generates schema.gql file from 'GQLRootResolver'
toGraphQLDocument
  :: RootResCon m event query mut sub
  => proxy (GQLRootResolver m event query mut sub)
  -> ByteString
toGraphQLDocument x = case fullSchema x of
  Failure errors       -> pack (show errors)
  Success lib warnings -> renderGraphQLDocument lib

toMorpheusHaskellAPi :: String -> ByteString -> Either ByteString ByteString
toMorpheusHaskellAPi moduleName doc = case parseGraphQLDocument doc of
  Failure errors       -> Left $ pack (show errors)
  Success lib warnings -> Right $ renderHaskellDocument moduleName lib

importGQLDocument :: String -> Q [Dec]
importGQLDocument src = runIO (readFile src) >>= compileDocument False

importGQLDocumentWithNamespace :: String -> Q [Dec]
importGQLDocumentWithNamespace src =
  runIO (readFile src) >>= compileDocument True
