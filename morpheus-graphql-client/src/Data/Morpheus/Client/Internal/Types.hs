{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
    ClientDefinition (..),
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

data ClientConstructorDefinition k = ConsD
  { cName :: TypeName,
    cFields :: [FieldDefinition k VALID]
  }
  deriving (Show)

data ClientTypeDefinition = ClientTypeDefinition
  { clientTypeName :: TypeNameTH,
    clientCons :: [ClientConstructorDefinition ANY],
    clientKind :: TypeKind
  }
  deriving (Show)

data ClientDefinition = ClientDefinition
  { clientArguments :: Maybe ClientTypeDefinition,
    clientTypes :: [ClientTypeDefinition]
  }
  deriving (Show)
