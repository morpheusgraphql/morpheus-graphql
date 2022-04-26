{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare.Client
  ( declareClient,
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
  ( ClientDefinition (..),
    ClientTypeDefinition (..),
    Mode (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Client.Internal.Utils (withMode)
import Data.Morpheus.CodeGen.Internal.TH (toCon)
import Language.Haskell.TH
import Relude hiding (Type)

declareClient :: Mode -> String -> ClientDefinition -> Q [Dec]
declareClient _ _ ClientDefinition {clientTypes = []} = pure []
declareClient mode src ClientDefinition {clientArguments, clientTypes = rootType : subTypes} =
  (<>)
    <$> defineOperationType
      (queryArgumentType clientArguments)
      src
      rootType
    <*> (concat <$> traverse declareType (withMode mode subTypes))

declareType :: ClientTypeDefinition -> Q [Dec]
declareType clientType@ClientTypeDefinition {clientKind} = do
  types <- typeDeclarations clientKind clientType
  instances <- aesonDeclarations clientKind clientType
  pure (types <> instances)

queryArgumentType :: Maybe ClientTypeDefinition -> (Type, Q [Dec])
queryArgumentType Nothing = (toCon ("()" :: String), pure [])
queryArgumentType (Just client@ClientTypeDefinition {clientTypeName}) =
  (toCon (typename clientTypeName), declareType client)

defineOperationType :: (Type, Q [Dec]) -> String -> ClientTypeDefinition -> Q [Dec]
defineOperationType
  (argType, argumentTypes)
  query
  clientType@ClientTypeDefinition
    { clientTypeName = TypeNameTH {typename}
    } =
    do
      rootType <- declareType clientType
      typeClassFetch <- deriveFetch argType typename query
      argsT <- argumentTypes
      pure $ rootType <> typeClassFetch <> argsT
