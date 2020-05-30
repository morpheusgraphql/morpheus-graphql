module Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
    ClientDefinition (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    IN,
    TypeDefinition,
    TypeKind,
    TypeNameTH (..),
  )

data ClientTypeDefinition = ClientTypeDefinition
  { clientTypeName :: TypeNameTH,
    clientCons :: [ConsD ANY],
    clientArgTypes :: [ClientTypeDefinition],
    clientKind :: TypeKind
    --clientOriginalType :: TypeDefinition ANY
  }
  deriving (Show)

data ClientDefinition = ClientDefinition
  { clientArguments :: Maybe ClientTypeDefinition,
    clientTypes :: [ClientTypeDefinition]
  }
  deriving (Show)
