{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare
  ( declareGlobalTypes,
    declareGlobalTypesByName,
    declareLocalTypes,
    declareLocalTypesInline,
    internalLegacyLocalDeclareTypes,
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

internalLegacyLocalDeclareTypes :: IO SchemaSource -> ExecutableSource -> Q [Dec]
internalLegacyLocalDeclareTypes schemaSrc query = do
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
        toLocalDefinitions executableDoc schemaDoc
    )
    ( \(fetch, types) ->
        (<>)
          <$> declareFetch query fetch
          <*> declareTypes types
    )

globalTypeDeclarations :: SchemaSource -> (TypeName -> Bool) -> Q [Dec]
globalTypeDeclarations src f = handleResult (toGlobalDefinitions f <$> parseSchema src) declareTypes

-- | declares global or local types, depending
-- on whether the second argument is specified or not
clientTypeDeclarations ::
  SchemaSource ->
  Maybe ExecutableSource ->
  Q [Dec]
clientTypeDeclarations src (Just doc) = internalLegacyLocalDeclareTypes (pure src) doc
clientTypeDeclarations src Nothing = globalTypeDeclarations src (const True)

{- ORMOLU_DISABLE -}
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
  FilePath  -- ^ the schema path relative to the  project location,
  -- both introspection (.json) and
  -- schema definition (.gql, .graphql) are accepted.
  -> Q [Dec]
declareGlobalTypes = flip declareClientTypes Nothing
{- ORMOLU_ENABLE -}

-- | declares global types like 'declareGlobalTypes',
-- while enabling to select only the types that are needed.
declareGlobalTypesByName :: FilePath -> [TypeName] -> Q [Dec]
declareGlobalTypesByName path names = do
  schema <- getSource path
  globalTypeDeclarations schema (`member` S.fromList names)

{- ORMOLU_DISABLE -}
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
  FilePath -- ^  the schema path relative to the  project location.
  -- both introspection (`.json`) and
  -- schema definition (`.gql`, `.graphql`) are accepted.
  -> FilePath -- ^ query path relative to the  project location
  -> Q [Dec]
declareLocalTypes schema query = declareClientTypes schema (Just query)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
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
  FilePath -- ^ the schema path relative to the  project location.
  -- both introspection (`.json`) and
  -- schema definition (`.gql`, `.graphql`) are accepted.
  -> ExecutableSource -- ^ inline graphql query in Text format
  -> Q [Dec]
declareLocalTypesInline schemaPath query = do
  schema <- getSource schemaPath
  clientTypeDeclarations schema (Just query)
{- ORMOLU_ENABLE -}

declareClientTypes ::
  FilePath ->
  Maybe FilePath ->
  Q [Dec]
declareClientTypes schemaPath queryPath = do
  schema <- getSource schemaPath
  query <- traverse getFile queryPath
  clientTypeDeclarations schema query
