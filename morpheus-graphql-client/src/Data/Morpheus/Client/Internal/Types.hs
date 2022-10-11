{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    RequestTypeDefinition (..),
    FetchError (..),
    SchemaSource (..),
    ExecutableSource,
    GQLClientResult,
    ClientDeclaration (..),
    DERIVING_MODE (..),
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenType,
    CodeGenTypeName,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    OperationType,
    TypeKind,
    TypeName,
  )
import Relude hiding (ByteString)

data DERIVING_MODE = SCALAR_MODE | ENUM_MODE | TYPE_MODE

data ClientDeclaration
  = ToJSONClass DERIVING_MODE CodeGenType
  | FromJSONClass DERIVING_MODE CodeGenType
  | RequestTypeClass RequestTypeDefinition
  | ClientType CodeGenType

data ClientTypeDefinition = ClientTypeDefinition
  { clientTypeName :: CodeGenTypeName,
    clientCons :: [CodeGenConstructor],
    clientKind :: TypeKind
  }
  deriving (Show)

data RequestTypeDefinition = RequestTypeDefinition
  { requestName :: TypeName,
    requestArgs :: TypeName,
    requestType :: OperationType,
    requestQuery :: String
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
