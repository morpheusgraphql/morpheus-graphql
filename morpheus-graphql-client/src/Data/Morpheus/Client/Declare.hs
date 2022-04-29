{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare
  ( declareTypesLegacy,
    clientTypeDeclarations,
    declareClientTypes,
    declareClientTypesInline,
  )
where

import Data.Morpheus.Client.Declare.Client
  ( declareFetch,
    declareTypes,
  )
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
import qualified Data.Text as T
import Language.Haskell.TH (Dec, Q, runIO)
import Relude

declareTypesLegacy :: IO SchemaSource -> Mode -> ExecutableSource -> Q [Dec]
declareTypesLegacy schemaSrc mode querySrc = do
  schemaText <- runIO schemaSrc
  let request =
        GQLRequest
          { query = querySrc,
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
          <$> declareFetch (T.unpack querySrc) fetch
          <*> declareTypes types
    )

clientTypeDeclarations :: SchemaSource -> Maybe ExecutableSource -> Q [Dec]
clientTypeDeclarations src (Just doc) = declareTypesLegacy (pure src) Local doc
clientTypeDeclarations src Nothing = do
  handleResult (parseSchema src >>= toGlobalDefinitions) declareTypes

declareClientTypesInline :: Q FilePath -> Maybe Text -> Q [Dec]
declareClientTypesInline schemaPath queryText = do
  src <- getSource schemaPath
  clientTypeDeclarations src queryText

declareClientTypes :: Q FilePath -> Maybe (Q FilePath) -> Q [Dec]
declareClientTypes schemaPath queryPath = do
  queryText <- traverse getFile queryPath
  declareClientTypesInline schemaPath queryText
