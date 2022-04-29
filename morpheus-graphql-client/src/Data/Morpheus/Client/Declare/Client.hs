{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare.Client
  ( declareFetch,
    declareTypes,
  )
where

import Data.Morpheus.Client.Declare.Aeson
  ( aesonDeclarations,
  )
import Data.Morpheus.Client.Declare.Type
  ( typeDeclarations,
  )
import Data.Morpheus.Client.Fetch
  ( deriveFetch,
  )
import Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    FetchDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.CodeGen.Internal.TH (toCon)
import Language.Haskell.TH
import Relude hiding (Type)

declareFetch :: String -> FetchDefinition -> Q [Dec]
declareFetch query FetchDefinition {clientArgumentsTypeName, rootTypeName} = do
  deriveFetch (argumentType clientArgumentsTypeName) (typename rootTypeName) query

declareTypes :: [ClientTypeDefinition] -> Q [Dec]
declareTypes subTypes = concat <$> traverse declareType subTypes

declareType :: ClientTypeDefinition -> Q [Dec]
declareType clientType@ClientTypeDefinition {clientKind} = do
  types <- typeDeclarations clientKind clientType
  instances <- aesonDeclarations clientKind clientType
  pure (types <> instances)

argumentType :: Maybe TypeNameTH -> Type
argumentType Nothing = toCon ("()" :: String)
argumentType (Just clientTypeName) = toCon (typename clientTypeName)
