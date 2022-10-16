{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Printing.TH
  ( printDeclarations,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    withObject,
  )
import Data.Morpheus.Client.Fetch.RequestType
  ( RequestType (..),
  )
import Data.Morpheus.Client.Internal.AST
  ( ClientDeclaration (..),
    DERIVING_MODE (..),
    RequestTypeDefinition (..),
  )
import Data.Morpheus.Client.Internal.TH
  ( declareIfNotDeclared,
    decodeObjectE,
    deriveIfNotDefined,
    failExp,
    matchWith,
    mkFieldsE,
  )
import Data.Morpheus.Client.Internal.Utils
  ( emptyTypeError,
    omitNulls,
    takeValueType,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenType (..),
    CodeGenTypeName (..),
    TypeClassInstance (..),
    getFullName,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintDec (printDec),
    ToString (..),
    destructConstructor,
    toCon,
    toName,
    toString,
    toVar,
    v',
    _',
  )
import Data.Morpheus.Types.GQLScalar
  ( scalarFromJSON,
    scalarToJSON,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeName,
  )
import Language.Haskell.TH (Dec, DecQ, Exp (..), ExpQ, Name, PatQ, Q, Type, appE, tupP)
import Relude hiding (ToString, Type, toString)

printDeclarations :: [ClientDeclaration] -> Q [Dec]
printDeclarations clientType = concat <$> traverse typeDeclarations clientType

typeDeclarations :: ClientDeclaration -> Q [Dec]
typeDeclarations (FromJSONClass mode clientDef) = deriveIfNotDefined (deriveFromJSON mode) ''FromJSON clientDef
typeDeclarations (ToJSONClass mode clientDef) = deriveIfNotDefined (deriveToJSON mode) ''ToJSON clientDef
typeDeclarations (ClientType c) = declareIfNotDeclared printDec c
typeDeclarations (RequestTypeClass RequestTypeDefinition {..}) = do
  x <- printDec $ TypeClassInstance ''RequestType [] (toCon requestName :: Type) [(''RequestArgs, toCon requestArgs)] methods
  pure [x]
  where
    methods =
      [ ('__name, ([_'], [|requestName|])),
        ('__query, ([_'], [|requestQuery|])),
        ('__type, ([_'], [|requestType|]))
      ]

-- UTILS
type Methods = [(Name, ([PatQ], ExpQ))]

mkFromJSON :: CodeGenTypeName -> ExpQ -> DecQ
mkFromJSON name expr = do
  (tName :: Type) <- toCon (toName name)
  printDec $ TypeClassInstance ''FromJSON [] tName [] ([('parseJSON, ([], expr))] :: Methods)

mkToJSON :: CodeGenTypeName -> [PatQ] -> ExpQ -> DecQ
mkToJSON name args expr = do
  printDec $ TypeClassInstance ''ToJSON [] (toCon $ toName name :: Type) [] ([('toJSON, (args, expr))] :: Methods)

originalLit :: ToString TypeName a => CodeGenTypeName -> Q a
originalLit = toString . typename

-- FromJSON
deriveFromJSON :: DERIVING_MODE -> CodeGenType -> DecQ
deriveFromJSON SCALAR_MODE CodeGenType {cgTypeName} = mkFromJSON cgTypeName [|scalarFromJSON|]
deriveFromJSON _ CodeGenType {cgConstructors = [], ..} = emptyTypeError cgTypeName
deriveFromJSON ENUM_MODE CodeGenType {..} =
  mkFromJSON cgTypeName (matchWith (Just (v', failExp)) fromJSONEnum cgConstructors)
deriveFromJSON _ CodeGenType {cgConstructors = [cons], ..} =
  mkFromJSON cgTypeName (fromJSONObject cons)
deriveFromJSON _ typeD@CodeGenType {cgTypeName} =
  mkFromJSON cgTypeName (fromJSONUnion typeD)

fromJSONEnum :: CodeGenConstructor -> (PatQ, ExpQ)
fromJSONEnum CodeGenConstructor {constructorName} = (originalLit constructorName, appE (toVar 'pure) (toCon constructorName))

fromJSONObject :: CodeGenConstructor -> ExpQ
fromJSONObject con@CodeGenConstructor {constructorName} = withBody <$> decodeObjectE con
  where
    withBody body = AppE (AppE (VarE 'withObject) name) (LamE [v'] body)
    name :: Exp
    name = toString (getFullName constructorName)

fromJSONUnion :: CodeGenType -> ExpQ
fromJSONUnion CodeGenType {..} = appE (toVar 'takeValueType) (matchWith elseCondition f cgConstructors)
  where
    elseCondition =
      (tupP [_', v'],) . decodeObjectE
        <$> find ((typename cgTypeName ==) . typename . constructorName) cgConstructors
    f cons@CodeGenConstructor {..} =
      ( tupP [originalLit constructorName, if null constructorFields then _' else v'],
        decodeObjectE cons
      )

-- ToJSON
deriveToJSON :: DERIVING_MODE -> CodeGenType -> DecQ
deriveToJSON SCALAR_MODE CodeGenType {..} = mkToJSON cgTypeName [] [|scalarToJSON|]
deriveToJSON _ CodeGenType {cgConstructors = [], ..} = emptyTypeError cgTypeName
deriveToJSON ENUM_MODE CodeGenType {..} =
  mkToJSON cgTypeName [] (matchWith Nothing toJSONEnum cgConstructors)
deriveToJSON _ CodeGenType {cgConstructors = [cons], ..} =
  mkToJSON cgTypeName [destructConstructor cons] (toJSONObject cons)
deriveToJSON _ _ = fail "Input Unions are not yet supported"

toJSONEnum :: CodeGenConstructor -> (PatQ, ExpQ)
toJSONEnum CodeGenConstructor {constructorName} = (toCon constructorName, originalLit constructorName)

toJSONObject :: CodeGenConstructor -> ExpQ
toJSONObject CodeGenConstructor {..} = pure $ AppE (VarE 'omitNulls) (mkFieldsE constructorName '(.=) constructorFields)
