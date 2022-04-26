{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
    ClientDefinition (..),
    ClientConstructorDefinition (..),
    FetchError (..),
    Mode (..),
    Source (..),
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldDefinition,
    FieldName,
    GQLErrors,
    TypeKind,
    TypeName,
    VALID,
  )
import Relude hiding (ByteString)

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

data FetchError a
  = FetchErrorParseFailure String
  | FetchErrorProducedErrors GQLErrors (Maybe a)
  | FetchErrorNoResult
  deriving (Show, Eq, Generic)

data Mode = Local | Global | Both deriving (Show, Eq)

data Source
  = JSON ByteString
  | GQL ByteString
  deriving (Show, Eq)
