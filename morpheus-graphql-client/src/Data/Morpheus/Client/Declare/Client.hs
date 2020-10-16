{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    TypeNameTH (..),
  )
import Data.Morpheus.Internal.TH (toCon)
import Language.Haskell.TH
import Relude hiding (Type)

declareClient :: String -> ClientDefinition -> Q [Dec]
declareClient _ ClientDefinition {clientTypes = []} = pure []
declareClient src ClientDefinition {clientArguments, clientTypes = rootType : subTypes} =
  (<>)
    <$> defineOperationType
      (queryArgumentType clientArguments)
      src
      rootType
    <*> (concat <$> traverse declareType subTypes)

declareType :: ClientTypeDefinition -> Q [Dec]
declareType clientType@ClientTypeDefinition {clientKind} =
  apply clientType (typeDeclarations clientKind <> aesonDeclarations clientKind)

apply :: Applicative f => a -> [a -> f b] -> f [b]
apply a = traverse (\f -> f a)

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
