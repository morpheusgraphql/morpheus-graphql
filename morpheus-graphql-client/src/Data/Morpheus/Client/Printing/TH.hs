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
    ToJSON (toJSON),
    withObject,
  )
import Data.Morpheus.Client.Fetch.RequestType
  ( RequestType (..),
  )
import Data.Morpheus.Client.Internal.AST
  ( ClientDeclaration (..),
    ClientMethod (..),
    DERIVING_MODE (..),
    RequestTypeDefinition (..),
  )
import Data.Morpheus.Client.Internal.TH
  ( declareIfNotDeclared,
    decodeObjectE,
    deriveIfNotDefined,
    failExp,
    matchWith,
    originalLit,
    toJSONEnumMethod,
    toJSONObjectMethod,
  )
import Data.Morpheus.Client.Internal.Utils
  ( emptyTypeError,
    takeValueType,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( AssociatedType (AssociatedTypeName),
    CodeGenConstructor (..),
    CodeGenType (..),
    CodeGenTypeName (..),
    MethodArgument (..),
    TypeClassInstance (..),
    fromTypeName,
    getFullName,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintDec (printDec),
    ToName (toName),
    ToString (..),
    toCon,
    toString,
    toVar,
    v',
    _',
  )
import Data.Morpheus.Types.GQLScalar
  ( scalarFromJSON,
    scalarToJSON,
  )
import Language.Haskell.TH (Dec, DecQ, Exp (..), ExpQ, PatQ, Q, appE, tupP)
import Relude hiding (ToString, Type, toString)

printDeclarations :: [ClientDeclaration] -> Q [Dec]
printDeclarations clientType = concat <$> traverse typeDeclarations clientType

typeDeclarations :: ClientDeclaration -> Q [Dec]
typeDeclarations (FromJSONClass mode clientDef) = deriveIfNotDefined (deriveFromJSON mode) ''FromJSON clientDef
typeDeclarations (ToJSONClass mode clientDef) = deriveIfNotDefined (deriveToJSON mode) ''ToJSON clientDef
typeDeclarations (ClientType c) = declareIfNotDeclared printDec c
typeDeclarations (RequestTypeClass RequestTypeDefinition {..}) = do
  pure
    <$> printDec
      TypeClassInstance
        { typeClassName = ''RequestType,
          typeClassContext = [],
          typeClassTarget = fromTypeName requestName,
          assoc = [(''RequestArgs, AssociatedTypeName $ toName requestArgs)],
          typeClassMethods
        }
  where
    typeClassMethods =
      [ ('__name, ProxyArgument, ClientMethod [|requestName|]),
        ('__query, ProxyArgument, ClientMethod [|requestQuery|]),
        ('__type, ProxyArgument, ClientMethod [|requestType|])
      ]

mkFromJSON :: CodeGenTypeName -> ExpQ -> DecQ
mkFromJSON name expr =
  printDec
    TypeClassInstance
      { typeClassName = ''FromJSON,
        typeClassContext = [],
        typeClassTarget = name,
        assoc = [],
        typeClassMethods = [('parseJSON, NoArgument, ClientMethod expr)]
      }

mkToJSON :: CodeGenTypeName -> MethodArgument -> ExpQ -> DecQ
mkToJSON name args expr =
  printDec
    TypeClassInstance
      { typeClassName = ''ToJSON,
        typeClassContext = [],
        typeClassTarget = name,
        assoc = [],
        typeClassMethods = [('toJSON, args, ClientMethod expr)]
      }

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
deriveToJSON SCALAR_MODE CodeGenType {..} = mkToJSON cgTypeName NoArgument [|scalarToJSON|]
deriveToJSON _ CodeGenType {cgConstructors = [], ..} = emptyTypeError cgTypeName
deriveToJSON ENUM_MODE CodeGenType {..} = mkToJSON cgTypeName NoArgument (toJSONEnumMethod cgConstructors)
deriveToJSON _ CodeGenType {cgConstructors = [cons], ..} = mkToJSON cgTypeName (DestructArgument cons) (toJSONObjectMethod cons)
deriveToJSON _ _ = fail "Input Unions are not yet supported"
