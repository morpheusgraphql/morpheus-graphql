{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare
  ( declareTypesLegacy,
    declareClientTypes,
    declareClientTypesInline,
  )
where

import Data.Morpheus.Client.Declare.Client
  ( declareTypes,
  )
import Data.Morpheus.Client.Declare.Fetch
import Data.Morpheus.Client.Internal.Types
  ( ExecutableSource,
    Mode (Local),
    SchemaSource,
  )
import Data.Morpheus.Client.Internal.Utils (getFile, getSource, handleResult)
import Data.Morpheus.Client.Schema.Parse (parseSchema)
import Data.Morpheus.Client.Transform
  ( toGlobalDefinitions,
    toLocalDefinitions,
  )
import Data.Morpheus.Core (parseRequest)
import Data.Morpheus.Types.IO (GQLRequest (..))
import Language.Haskell.TH (Dec, Q, runIO)
import Relude

declareTypesLegacy :: IO SchemaSource -> Mode -> ExecutableSource -> Q [Dec]
declareTypesLegacy schemaSrc mode query = do
  schemaText <- runIO schemaSrc
  let request =
        GQLRequest
          { query,
            operationName = Nothing,
            variables = Nothing
          }
  handleResult
    ( do
        schemaDoc <- parseSchema schemaText
        executableDoc <- parseRequest request
        toLocalDefinitions mode executableDoc schemaDoc
    )
    ( \(fetch, types) ->
        (<>)
          <$> declareFetch query fetch
          <*> declareTypes types
    )

clientTypeDeclarations :: SchemaSource -> Maybe ExecutableSource -> Q [Dec]
clientTypeDeclarations src (Just doc) = declareTypesLegacy (pure src) Local doc
clientTypeDeclarations src Nothing = do
  handleResult (parseSchema src >>= toGlobalDefinitions) declareTypes

declareClientTypesInline :: FilePath -> ExecutableSource -> Q [Dec]
declareClientTypesInline schemaPath query = do
  schema <- getSource schemaPath
  clientTypeDeclarations schema (Just query)

declareClientTypes :: FilePath -> Maybe FilePath -> Q [Dec]
declareClientTypes schemaPath queryPath = do
  schema <- getSource schemaPath
  query <- traverse getFile queryPath
  clientTypeDeclarations schema query
