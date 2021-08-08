{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
    ClientDefinition (..),
    ClientConstructorDefinition (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldDefinition,
    FieldName,
    TypeKind,
    TypeName,
    VALID,
  )
import Relude

data TypeNameTH = TypeNameTH
  { namespace :: [FieldName],
    typename :: TypeName
  }
  deriving (Show)

data ClientConstructorDefinition = ClientConstructorDefinition
  { cName :: TypeName,
    cFields :: [FieldDefinition ANY VALID]
  }
  deriving (Show)

data ClientTypeDefinition = ClientTypeDefinition
  { clientTypeName :: TypeNameTH,
    clientCons :: [ClientConstructorDefinition],
    clientKind :: TypeKind
  }
  deriving (Show)

data ClientDefinition = ClientDefinition
  { clientArguments :: Maybe ClientTypeDefinition,
    clientTypes :: [ClientTypeDefinition]
  }
  deriving (Show)
