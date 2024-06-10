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
    RequestTypeDefinition (..),
    UnionPat (..),
    ClientTypeDefinition (..),
  )
where

import Data.Aeson (parseJSON)
import Data.Foldable (foldr1)
import Data.Morpheus.Client.CodeGen.Internal
  ( withObject,
    withUnion,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenType,
    CodeGenTypeName,
    PrintableValue (..),
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
import Prettyprinter
  ( Doc,
    Pretty (..),
    indent,
    line,
    space,
    vsep,
    (<+>),
  )
import Relude hiding (lift, show, toString)
import Prelude (show)

data DERIVING_MODE = SCALAR_MODE | ENUM_MODE | TYPE_MODE

data ClientDeclaration
  = InstanceDeclaration DERIVING_MODE (TypeClassInstance ClientMethod)
  | ClientTypeDeclaration CodeGenType

data ClientPreDeclaration
  = ToJSONClass DERIVING_MODE CodeGenType
  | FromJSONClass DERIVING_MODE CodeGenType
  | FromJSONUnionClass CodeGenTypeName [(UnionPat, (CodeGenTypeName, Maybe String))]
  | FromJSONObjectClass CodeGenTypeName CodeGenConstructor
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
  pretty (InstanceDeclaration _ def) = pretty def

data ClientMethod
  = PrintableMethod PrintableValue
  | FunctionNameMethod Name
  | MatchMethod ValueMatch
  | ToJSONObjectMethod Name [(FieldName, Name, Name)]
  | FromJSONObjectMethod TypeName [AesonField]
  | FromJSONUnionMethod [([UnionPat], (Name, Maybe Name))]

type AesonField = (Name, Name, FieldName)

instance Pretty ClientMethod where
  pretty (FunctionNameMethod x) = space <> printTHName x
  pretty (PrintableMethod x) = space <> pretty x
  pretty (MatchMethod x) = space <> printMatchDoc x
  pretty (ToJSONObjectMethod name fields) = line <> indent 2 (printTHName name <> line <> indent 2 (list (map mkEntry fields)))
    where
      mkEntry (n, o, v) = prettyLit n <+> printTHName o <+> printTHName v
  pretty (FromJSONObjectMethod name xs) = withBody $ printObjectDoc (toName name, xs)
    where
      withBody body = line <> indent 2 "withObject" <+> prettyLit name <+> "(\\v ->" <+> body <> ")"
  pretty (FromJSONUnionMethod xs) = line <> indent 2 ("withUnion" <> line <> indent 2 (tuple [indent 1 (matchDoc $ map toMatch xs) <> line]))
    where
      toMatch (pat, expr) = (tuple $ map mapP pat, printVariantDoc expr)
      mapP (UString v) = prettyLit v
      mapP (UVar v) = pretty v

list :: (Foldable t) => t (Doc ann) -> Doc ann
list xs = "[" <> indent 1 (foldr1 (\a b -> a <> "," <> line <> b) xs) <> line <> "]"

tuple :: (Foldable t) => t (Doc ann) -> Doc ann
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
  printExp (FromJSONUnionMethod matches) = appE (toVar 'withUnion) (matchExp $ map toMatch matches)
    where
      toMatch (pat, expr) = (tupP $ map mapP pat, printVariantExp expr)
      --
      mapP (UString v) = toString v
      mapP (UVar v) = toVar v

printVariantExp :: (Name, Maybe Name) -> ExpQ
printVariantExp (con, Just x) = uInfixE (toCon con) [|(<$>)|] (appE (toVar 'parseJSON) (toVar x))
printVariantExp (con, Nothing) = appE [|pure|] (toCon con)

printVariantDoc :: (Name, Maybe Name) -> Doc n
printVariantDoc (con, Just x) = printTHName con <+> "<$>" <+> "parseJSON" <+> printTHName x
printVariantDoc (con, Nothing) = "pure" <+> printTHName con

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

prettyLit :: (Show a) => a -> Doc ann
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
matchDoc = (("\\case" <> line) <>) . indent 2 . vsep . map buildMatch
  where
    buildMatch (pat, fb) = pat <+> "->" <+> fb
