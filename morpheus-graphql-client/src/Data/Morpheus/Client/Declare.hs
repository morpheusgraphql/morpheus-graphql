{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare
  ( declareGlobalTypes,
    declareLocalTypes,
    declareLocalTypesInline,
    internalLegacyDeclareTypes,
    clientTypeDeclarations,
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

internalLegacyDeclareTypes :: IO SchemaSource -> Mode -> ExecutableSource -> Q [Dec]
internalLegacyDeclareTypes schemaSrc mode query = do
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

-- | declares global or local types, depending
-- on whether the second argument is specified or not
clientTypeDeclarations ::
  SchemaSource ->
  Maybe ExecutableSource ->
  Q [Dec]
clientTypeDeclarations src (Just doc) = internalLegacyDeclareTypes (pure src) Local doc
clientTypeDeclarations src Nothing = do
  handleResult (parseSchema src >>= toGlobalDefinitions) declareTypes

-- | declares input, enum and scalar types for specified schema
declareGlobalTypes ::
  -- | the schema path relative to the  project location,
  -- both introspection (`.json`) and
  -- schema definition (`.gql`, `.graphql`) are accepted.
  FilePath ->
  Q [Dec]
declareGlobalTypes = flip declareClientTypes Nothing

-- | declares object, interface and union types for
-- specified schema and query.
declareLocalTypes ::
  -- | the schema path relative to the  project location.
  -- both introspection (`.json`) and
  -- schema definition (`.gql`, `.graphql`) are accepted.
  FilePath ->
  -- | query path relative to the  project location
  FilePath ->
  Q [Dec]
declareLocalTypes schema query = declareClientTypes schema (Just query)

-- | inline version of `declareLocalTypes`
declareLocalTypesInline ::
  -- | the schema path relative to the  project location.
  -- both introspection (`.json`) and
  -- schema definition (`.gql`, `.graphql`) are accepted.
  FilePath ->
  -- | inline graphql query in Text format
  ExecutableSource ->
  Q [Dec]
declareLocalTypesInline schemaPath query = do
  schema <- getSource schemaPath
  clientTypeDeclarations schema (Just query)

declareClientTypes :: FilePath -> Maybe FilePath -> Q [Dec]
declareClientTypes schemaPath queryPath = do
  schema <- getSource schemaPath
  query <- traverse getFile queryPath
  clientTypeDeclarations schema query
