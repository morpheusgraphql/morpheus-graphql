{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare.Client
  ( declareTypes,
  )
where

import Data.Morpheus.Client.Declare.Aeson
  ( aesonDeclarations,
  )
import Data.Morpheus.Client.Declare.Type
  ( typeDeclarations,
  )
import Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
  )
import Language.Haskell.TH
import Relude hiding (Type)

declareTypes :: [ClientTypeDefinition] -> Q [Dec]
declareTypes subTypes = concat <$> traverse declareType subTypes

declareType :: ClientTypeDefinition -> Q [Dec]
declareType clientType@ClientTypeDefinition {clientKind} = do
  types <- typeDeclarations clientKind clientType
  instances <- aesonDeclarations clientKind clientType
  pure (types <> instances)
