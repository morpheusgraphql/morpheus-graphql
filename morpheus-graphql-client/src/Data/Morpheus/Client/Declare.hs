{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare
  ( declareGlobalTypes,
    declareGlobalTypesByName,
    declareLocalTypes,
    declareLocalTypesInline,
    internalLegacyDeclareTypes,
    clientTypeDeclarations,
    raw,
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
import Data.Morpheus.Client.QuasiQuoter (raw)
import Data.Morpheus.Client.Schema.Parse (parseSchema)
import Data.Morpheus.Client.Transform
  ( toGlobalDefinitions,
    toLocalDefinitions,
  )
import Data.Morpheus.CodeGen.Internal.AST
import Data.Morpheus.Core (parseRequest)
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Set
import qualified Data.Set as S
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

globalTypeDeclarations :: SchemaSource -> (TypeName -> Bool) -> Q [Dec]
globalTypeDeclarations src f = handleResult (parseSchema src >>= toGlobalDefinitions f) declareTypes

-- | declares global or local types, depending
-- on whether the second argument is specified or not
clientTypeDeclarations ::
  SchemaSource ->
  Maybe ExecutableSource ->
  Q [Dec]
clientTypeDeclarations src (Just doc) = internalLegacyDeclareTypes (pure src) Local doc
clientTypeDeclarations src Nothing = globalTypeDeclarations src (const True)

-- | declares input, enum and scalar types for specified schema
--
-- Example where the schema is defined in SDL format
--
-- @
-- 'declareGlobalTypes' "schema.gql"
-- @
--
-- Example with schema as introspection in JSON format.
--
-- @
-- 'declareGlobalTypes' "schema.json"
-- @
declareGlobalTypes ::
  -- | the schema path relative to the  project location,
  -- both introspection (.json) and
  -- schema definition (.gql, .graphql) are accepted.
  FilePath ->
  Q [Dec]
declareGlobalTypes = flip declareClientTypes Nothing

-- declares global types like 'declareGlobalTypes',
-- while enabling to select only the types that are needed.
declareGlobalTypesByName :: FilePath -> [TypeName] -> Q [Dec]
declareGlobalTypesByName path names = do
  schema <- getSource path
  globalTypeDeclarations schema (`member` S.fromList names)

-- | declares object, interface and union types for
-- specified schema and query.
--
-- Example where the schema is defined in SDL format
--
-- @
-- 'declareLocalTypes' "schema.gql" "query.gql"
-- @
--
-- Example with schema as introspection in JSON format.
--
-- @
-- 'declareLocalTypes' "schema.json" "query.gql"
-- @
declareLocalTypes ::
  -- | the schema path relative to the  project location.
  -- both introspection (`.json`) and
  -- schema definition (`.gql`, `.graphql`) are accepted.
  FilePath ->
  -- | query path relative to the  project location
  FilePath ->
  Q [Dec]
declareLocalTypes schema query = declareClientTypes schema (Just query)

-- | inline version of `declareLocalTypes`, however
-- instead of specifying the file path, you can simply
-- pass the query as text using QuasiQuoter `raw`
--
-- @
-- `declareLocalTypesInline` "schema.gql"
--     [`raw`|
--        query GetUsers {
--           users {
--             name
--           }
--        }
--     ]
--  @
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
