{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.AST where

import Data.Morpheus.Client.Internal.TH (toJSONEnumMethod, toJSONObjectMethod)
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenType,
    CodeGenTypeName,
    TypeClassInstance (typeClassName),
  )
import Data.Morpheus.CodeGen.TH (PrintExp (..))
import Data.Morpheus.Types.Internal.AST
  ( OperationType,
    TypeKind,
    TypeName,
  )
import Language.Haskell.TH (ExpQ, Name)
import Prettyprinter (Pretty (..))
import Relude

data DERIVING_MODE = SCALAR_MODE | ENUM_MODE | TYPE_MODE

data ClientDeclaration
  = InstanceDeclaration (TypeClassInstance ClientMethod)
  | ClientTypeDeclaration CodeGenType

data ClientPreDeclaration
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
  pretty (ClientTypeDeclaration def) = pretty def
  pretty (InstanceDeclaration def) = "-- TODO: " <> show (typeClassName def) <> " ToJSONClass\n"

data ClientMethod
  = ClientMethodExp ExpQ
  | ToJSONEnumMethod [CodeGenConstructor]
  | ToJSONObjectMethod CodeGenConstructor

instance PrintExp ClientMethod where
  printExp (ClientMethodExp x) = x
  printExp (ToJSONEnumMethod x) = toJSONEnumMethod x
  printExp (ToJSONObjectMethod x) = toJSONObjectMethod x
