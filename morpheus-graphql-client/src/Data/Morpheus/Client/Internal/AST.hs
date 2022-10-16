{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.AST where

import Data.Morpheus.Client.Internal.TH (toJSONEnumMethod, toJSONObjectMethod)
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenType (cgTypeName),
    CodeGenTypeName,
    getFullName,
  )
import Data.Morpheus.CodeGen.TH (PrintExp (..))
import Data.Morpheus.Types.Internal.AST
  ( OperationType,
    TypeKind,
    TypeName,
  )
import Language.Haskell.TH (ExpQ)
import Prettyprinter (Pretty (..))
import Relude

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

instance Pretty ClientDeclaration where
  pretty (ClientType def) = pretty def
  pretty (ToJSONClass _ def) = "-- TODO: " <> show (getFullName $ cgTypeName def) <> " ToJSONClass\n"
  pretty (FromJSONClass _ def) = "-- TODO: " <> show (getFullName $ cgTypeName def) <> " FromJSONClass\n"
  pretty (RequestTypeClass def) = "-- TODO: " <> show (requestName def) <> " RequestTypeClass\n"

data ClientMethod
  = ClientMethod ExpQ
  | ToJSONEnumMethod [CodeGenConstructor]
  | ToJSONObjectMethod CodeGenConstructor

instance PrintExp ClientMethod where
  printExp (ClientMethod x) = x
  printExp (ToJSONEnumMethod x) = toJSONEnumMethod x
  printExp (ToJSONObjectMethod x) = toJSONObjectMethod x
