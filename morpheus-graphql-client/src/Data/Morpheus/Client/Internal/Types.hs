{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
    FetchDefinition (..),
    ClientConstructorDefinition (..),
    FetchError (..),
    SchemaSource (..),
    ExecutableSource,
    GQLClientResult,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.CodeGen.Internal.AST (CodeGenField (..))
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLErrors,
    OperationType,
    TypeKind,
    TypeName,
  )
import Relude hiding (ByteString)

data TypeNameTH = TypeNameTH
  { namespace :: [FieldName],
    typename :: TypeName
  }
  deriving (Show)

data ClientConstructorDefinition = ClientConstructorDefinition
  { cName :: TypeName,
    cFields :: [CodeGenField]
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
    clientArgumentsTypeName :: Maybe TypeNameTH,
    fetchOperationType :: OperationType
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

type GQLClientResult (a :: Type) = (Either (FetchError a) a)
