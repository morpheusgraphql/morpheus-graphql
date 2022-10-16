{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.AST where

import Data.Morpheus.Client.Internal.TH
  ( MValue (..),
    ValueMatch,
    fromJSONObjectMethod,
    fromJSONUnionMethod,
    printMatchDec,
    toJSONObjectMethod,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenType,
    CodeGenTypeName,
    TypeClassInstance,
    printTHName,
  )
import Data.Morpheus.CodeGen.TH (PrintExp (..))
import Data.Morpheus.Types.Internal.AST (OperationType, TypeKind, TypeName, unpackName)
import Language.Haskell.TH (Name)
import Language.Haskell.TH.Lib (varE)
import Language.Haskell.TH.Syntax (Lift (..))
import Prettyprinter
  ( Doc,
    Pretty (..),
    indent,
    vsep,
    (<+>),
  )
import Relude hiding (lift, show)
import Prelude (show)

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
  | FunctionNameMethod Name
  | MatchMethod ValueMatch
  | ToJSONObjectMethod CodeGenConstructor
  | FromJSONObjectMethod CodeGenConstructor
  | FromJSONUnionMethod CodeGenType

instance Pretty ClientMethod where
  pretty (FunctionNameMethod x) = printTHName x
  pretty (PrintableMethod x) = pretty x
  pretty (MatchMethod x) = printMatch x
  pretty _ = "undefined -- TODO: should be real function"

prettyLit :: Show a => a -> Doc ann
prettyLit a = pretty (show a)

prettyName :: TypeName -> Doc ann
prettyName a = pretty (unpackName a :: Text)

printMatch :: [MValue] -> Doc n
printMatch = ("\\case " <>) . indent 4 . vsep . map buildMatch
  where
    buildMatch (MFrom a b) = prettyLit a <+> "-> pure" <+> prettyName b
    buildMatch (MTo a b) = prettyName a <+> "->" <+> prettyLit b
    buildMatch (MFunction v name) = pretty v <+> "->" <+> printTHName name <+> pretty v

instance PrintExp ClientMethod where
  printExp (FunctionNameMethod v) = varE v
  printExp (PrintableMethod v) = printExp v
  printExp (MatchMethod x) = printMatchDec x
  printExp (ToJSONObjectMethod x) = toJSONObjectMethod x
  printExp (FromJSONObjectMethod x) = fromJSONObjectMethod x
  printExp (FromJSONUnionMethod x) = fromJSONUnionMethod x
