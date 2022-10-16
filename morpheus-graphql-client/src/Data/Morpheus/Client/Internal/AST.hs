{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.AST where

import Data.Morpheus.Client.Internal.TH (toJSONEnumMethod, toJSONObjectMethod)
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenType,
    CodeGenTypeName,
    TypeClassInstance,
  )
import Data.Morpheus.CodeGen.TH (PrintExp (..))
import Data.Morpheus.Types.Internal.AST
  ( OperationType,
    TypeKind,
    TypeName,
  )
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (Lift (..))
import Prettyprinter (Pretty (..))
import Relude hiding (lift)

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
  pretty (InstanceDeclaration def) = pretty def

data Printable where
  Printable :: forall a. (Show a, Lift a) => a -> Printable

instance Pretty Printable where
  pretty (Printable x) = pretty (show x :: String)

instance PrintExp Printable where
  printExp (Printable x) = [|x|]

data ClientMethod
  = PrintableMethod Printable
  | ClientMethodExp ExpQ
  | ToJSONEnumMethod [CodeGenConstructor]
  | ToJSONObjectMethod CodeGenConstructor

instance Pretty ClientMethod where
  pretty (PrintableMethod x) = pretty x
  pretty _ = "undefined -- TODO: should be real function"

instance PrintExp ClientMethod where
  printExp (PrintableMethod v) = printExp v
  printExp (ClientMethodExp x) = x
  printExp (ToJSONEnumMethod x) = toJSONEnumMethod x
  printExp (ToJSONObjectMethod x) = toJSONObjectMethod x
