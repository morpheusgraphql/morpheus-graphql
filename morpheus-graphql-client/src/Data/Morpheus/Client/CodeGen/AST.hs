{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.AST
  ( AesonField,
    ClientDeclaration (..),
    ClientMethod (..),
    ClientPreDeclaration (..),
    DERIVING_MODE (..),
    MValue (..),
    Printable (..),
    RequestTypeDefinition (..),
    UnionPat (..),
    ClientTypeDefinition (..),
  )
where

import Data.Foldable (foldr1)
import Data.Morpheus.Client.CodeGen.Internal
  ( takeValueType,
    withObject,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenType,
    CodeGenTypeName,
    TypeClassInstance,
    printTHName,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintExp (..),
    ToName (toName),
    toCon,
    toString,
    toVar,
    v',
  )
import Data.Morpheus.Types.Internal.AST (FieldName, OperationType, TypeKind, TypeName, unpackName)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift (..))
import Prettyprinter
  ( Doc,
    Pretty (..),
    indent,
    line,
    list,
    tupled,
    vsep,
    (<+>),
  )
import Relude hiding (lift, show, toString)
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
  | ToJSONObjectMethod Name [(FieldName, Name, Name)]
  | FromJSONObjectMethod TypeName [AesonField]
  | FromJSONUnionMethod [([UnionPat], (Name, [AesonField]))]

type AesonField = (Name, Name, FieldName)

instance Pretty ClientMethod where
  pretty (FunctionNameMethod x) = printTHName x
  pretty (PrintableMethod x) = pretty x
  pretty (MatchMethod x) = printMatchDoc x
  pretty (ToJSONObjectMethod name fields) = printTHName name <+> indent 2 (line <+> list (map mkEntry fields))
    where
      mkEntry (n, o, v) = prettyLit n <+> printTHName o <+> printTHName v
  pretty (FromJSONObjectMethod name xs) = withBody $ printObjectDoc (toName name, xs)
    where
      withBody body = "withObject" <+> prettyLit name <+> "(\\v ->" <+> body <+> ")"
  pretty (FromJSONUnionMethod xs) = "takeValueType" <+> tupled [matchDoc (map toMatch xs)]
    where
      toMatch (pat, expr) = (tuple $ map mapP pat, printObjectDoc expr)
      mapP (UString v) = prettyLit v
      mapP (UVar v) = pretty v
      tuple ls = "(" <> foldr1 (\a b -> a <> "," <+> b) ls <> ")"

instance PrintExp ClientMethod where
  printExp (FunctionNameMethod v) = varE v
  printExp (PrintableMethod v) = printExp v
  printExp (MatchMethod p) = printMatchExp p
  printExp (ToJSONObjectMethod name fields) = appE (varE name) (listE $ map mkEntry fields)
    where
      mkEntry (n, o, v) = uInfixE (toString n) (toVar o) (toVar v)
  printExp (FromJSONObjectMethod name fields) = withBody $ printObjectExp (toName name, fields)
    where
      withBody body = appE (appE (toVar 'withObject) (toString name)) (lamE [v'] body)
  printExp (FromJSONUnionMethod matches) = appE (toVar 'takeValueType) (matchExp $ map toMatch matches)
    where
      toMatch (pat, expr) = (tupP $ map mapP pat, printObjectExp expr)
      --
      mapP (UString v) = toString v
      mapP (UVar v) = toVar v

printObjectExp :: (Name, [AesonField]) -> ExpQ
printObjectExp (con, fields)
  | null fields = appE [|pure|] (toCon con)
  | otherwise = uInfixE (toCon con) [|(<$>)|] $ foldr1 (\x -> uInfixE x [|(<*>)|]) (map printFieldExp fields)

printObjectDoc :: (Name, [AesonField]) -> Doc n
printObjectDoc (name, fields)
  | null fields = "pure" <+> printTHName name
  | otherwise = printTHName name <+> "<$>" <+> foldr1 (\a b -> a <+> "<*>" <+> b) (map printFieldDoc fields)

printFieldExp :: AesonField -> ExpQ
printFieldExp (v, o, str) = uInfixE (toVar v) (toVar o) (toString str)

printFieldDoc :: AesonField -> Doc n
printFieldDoc (v, o, l) = printTHName v <+> printTHName o <+> prettyLit l

prettyLit :: Show a => a -> Doc ann
prettyLit a = pretty (show a)

prettyName :: TypeName -> Doc ann
prettyName a = pretty (unpackName a :: Text)

data UnionPat
  = UString TypeName
  | UVar String

data MValue
  = MFrom TypeName TypeName
  | MTo TypeName TypeName
  | MFunction String Name

type ValueMatch = [MValue]

printMatchDoc :: ValueMatch -> Doc n
printMatchDoc = matchDoc . map buildMatch
  where
    buildMatch (MFrom a b) = (prettyLit a, "pure" <+> prettyName b)
    buildMatch (MTo a b) = (prettyName a, prettyLit b)
    buildMatch (MFunction v name) = (pretty v, printTHName name <+> pretty v)

printMatchExp :: ValueMatch -> ExpQ
printMatchExp = matchExp . map buildMatch
  where
    buildMatch (MFrom a b) = (toString a, appE (toVar 'pure) (toCon b))
    buildMatch (MTo a b) = (toCon a, toString b)
    buildMatch (MFunction v name) = (toVar v, appE (varE name) v')

matchExp :: [(PatQ, ExpQ)] -> ExpQ
matchExp xs = lamCaseE (map buildMatch xs)
  where
    buildMatch (pat, fb) = match pat (normalB fb) []

matchDoc :: [(Doc n, Doc n)] -> Doc n
matchDoc = ("\\case " <>) . indent 4 . vsep . map buildMatch
  where
    buildMatch (pat, fb) = pat <+> "->" <+> fb
