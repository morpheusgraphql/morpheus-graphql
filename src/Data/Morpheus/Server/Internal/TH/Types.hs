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
  ( ArgumentsDefinition,
    ConsD (..),
    Description,
    Directives,
    FieldDefinition,
    FieldName,
    TypeKind,
    TypeName,
    Value,
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
data ServerTypeDefinition cat s
  = ServerTypeDefinition
      { tName :: TypeName,
        tCons :: [ServerConsD cat s],
        tKind :: TypeKind,
        gqlTypeDescription :: Maybe Text,
        gqlTypeDescriptions :: Map Text Description,
        gqlTypeDirectives :: Map Text (Directives s),
        gqlTypeFieldContents :: Map FieldName (Maybe (Value s), Maybe (ArgumentsDefinition s))
      }
  | ServerInterfaceDefinition TypeName TypeName TypeName
  deriving (Show)

type ServerDec = Reader ServerDecContext

newtype ServerDecContext = ServerDecContext
  { namespace :: Bool
  }
