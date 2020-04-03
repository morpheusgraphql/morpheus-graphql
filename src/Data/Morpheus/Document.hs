{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Morpheus.Document
  ( toGraphQLDocument
  , gqlDocument
  , parseFullGQLDocument
  , importGQLDocument
  , importGQLDocumentWithNamespace
  , parseDSL
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
import           Data.Morpheus.Parsing.Document.TypeSystem
                                                ( parseSchema 
                                                )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( renderGraphQLDocument 
                                                )
import           Data.Morpheus.Schema.SchemaAPI ( defaultTypes )
import           Data.Morpheus.Types            ( GQLRootResolver )
import           Data.Morpheus.Types.Internal.AST
                                                ( Schema
                                                , createDataTypeLib
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Stateless
                                                , Result(..)
                                                )
import           Data.Morpheus.Validation.Document.Validation
                                                ( validatePartialDocument )

                                              
parseDSL :: ByteString -> Either String Schema
parseDSL doc = case parseGraphQLDocument doc of
  Failure errors     -> Left (show errors)
  Success { result } -> Right result


parseDocument :: Text -> Stateless Schema
parseDocument doc =
  parseSchema doc >>= validatePartialDocument >>= createDataTypeLib

parseGraphQLDocument :: ByteString -> Stateless Schema
parseGraphQLDocument x = parseDocument (LT.toStrict $ decodeUtf8 x)

parseFullGQLDocument :: ByteString -> Stateless Schema
parseFullGQLDocument = parseGraphQLDocument >=> defaultTypes

-- | Generates schema.gql file from 'GQLRootResolver'
toGraphQLDocument
  :: RootResCon m event query mut sub
  => proxy (GQLRootResolver m event query mut sub)
  -> ByteString
toGraphQLDocument x = case fullSchema x of
  Failure errors           -> pack (show errors)
  Success { result = lib } -> renderGraphQLDocument lib



importGQLDocument :: String -> Q [Dec]
importGQLDocument src = runIO (readFile src) >>= compileDocument False

importGQLDocumentWithNamespace :: String -> Q [Dec]
importGQLDocumentWithNamespace src =
  runIO (readFile src) >>= compileDocument True
