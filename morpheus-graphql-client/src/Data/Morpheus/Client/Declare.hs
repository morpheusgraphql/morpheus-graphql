{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare
  ( declareTypesLegacy,
    declareClientTypesIO,
    declareClientTypes,
  )
where

import Data.Morpheus.Client.Declare.Client
  ( declareFetch,
    declareTypes,
  )
import Data.Morpheus.Client.Internal.Types
  ( ExecutableClientDocument,
    Mode (Local),
    Source,
  )
import Data.Morpheus.Client.Internal.Utils (getSource, handleResult)
import Data.Morpheus.Client.Schema.Parse (parseSchema)
import Data.Morpheus.Client.Transform
  ( toGlobalDefinitions,
    toLocalDefinitions,
  )
import Language.Haskell.TH (Dec, Q, runIO)
import Relude

declareTypesLegacy :: IO Source -> Mode -> ExecutableClientDocument -> Q [Dec]
declareTypesLegacy doc mode (query, source) = do
  schema <- runIO (parseSchema <$> doc)
  handleResult
    (schema >>= toLocalDefinitions mode query)
    ( \(fetch, types) ->
        (<>)
          <$> declareFetch source fetch
          <*> declareTypes types
    )

declareClientTypesIO :: IO Source -> Maybe ExecutableClientDocument -> Q [Dec]
declareClientTypesIO src (Just doc) = declareTypesLegacy src Local doc
declareClientTypesIO src Nothing = do
  schema <- runIO (parseSchema <$> src)
  handleResult (schema >>= toGlobalDefinitions) declareTypes

declareClientTypes :: Q FilePath -> Maybe ExecutableClientDocument -> Q [Dec]
declareClientTypes schemaPath doc = do
  src <- getSource schemaPath
  declareClientTypesIO (pure src) doc
