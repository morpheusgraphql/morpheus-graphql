module Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
  )
where

import Data.Morpheus.Types.Internal.AST

data ClientTypeDefinition = ClientTypeDefinition
  { clientTypeName :: NameTH,
    clientCons :: [ConsD IN],
    clientArgTypes :: [ClientTypeDefinition],
    clientOriginalType :: TypeDefinition ANY
  }
  deriving (Show)
