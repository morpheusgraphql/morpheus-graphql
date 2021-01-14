{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
    ServerDec,
    ServerDecContext (..),
    ServerConsD,
    ServerFieldDefinition (..),
    toServerField,
  )
where

import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    FieldDefinition,
    IN,
    TypeDefinition,
    TypeKind,
    TypeName,
  )
import Relude

data ServerFieldDefinition cat s = ServerFieldDefinition
  { isParametrized :: Bool,
    argumentsTypeName :: Maybe TypeName,
    originalField :: FieldDefinition cat s
  }
  deriving (Show)

toServerField :: FieldDefinition c s -> ServerFieldDefinition c s
toServerField = ServerFieldDefinition False Nothing

type ServerConsD cat s = ConsD (ServerFieldDefinition cat s)

--- Core
data ServerTypeDefinition cat s = ServerTypeDefinition
  { tName :: TypeName,
    typeArgD :: [ServerTypeDefinition IN s],
    tCons :: [ServerConsD cat s],
    tKind :: TypeKind,
    typeOriginal :: Maybe (TypeDefinition ANY s)
  }
  deriving (Show)

type ServerDec = Reader ServerDecContext

newtype ServerDecContext = ServerDecContext
  { namespace :: Bool
  }
