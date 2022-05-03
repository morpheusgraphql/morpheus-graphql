{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
    FetchDefinition (..),
    ClientConstructorDefinition (..),
    FetchError (..),
    SchemaSource (..),
    ExecutableSource,
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

data FetchDefinition = FetchDefinition
  { rootTypeName :: TypeNameTH,
    clientArgumentsTypeName :: Maybe TypeNameTH
  }
  deriving (Show)

data FetchError a
  = FetchErrorParseFailure String
  | FetchErrorProducedErrors GQLErrors (Maybe a)
  | FetchErrorNoResult
  deriving (Show, Eq, Generic)

data SchemaSource
  = JSON ByteString
  | GQL ByteString
  deriving (Show, Eq)

type ExecutableSource = Text
