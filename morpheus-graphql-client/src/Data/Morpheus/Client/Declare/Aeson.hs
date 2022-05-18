{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare.Aeson
  ( aesonDeclarations,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Morpheus.Client.Internal.TH
  ( decodeObjectE,
    destructRecord,
    failExp,
    matchWith,
    mkFieldsE,
    isTypeDeclared,
    hasInstance
  )
import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (..),
    ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.CodeGen.Internal.TH
  ( _',
    applyCons,
    camelCaseTypeName,
    funDSimple,
    toCon,
    toName,
    toString,
    v',
  )
import Data.Morpheus.Internal.Utils (IsMap (lookup))
import Data.Morpheus.Types.GQLScalar
  ( scalarFromJSON,
    scalarToJSON,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    Msg (msg),
    TypeKind (..),
    TypeName,
    internal,
    isResolverType,
  )
import qualified Data.Text as T
import Language.Haskell.TH
  ( DecQ,
    Exp (..),
    ExpQ,
    Name,
    PatQ,
    Q,
    appE,
    conP,
    cxt,
    instanceD,
    tupP,
 )
import Language.Haskell.TH.Syntax (Dec)
import Relude hiding (toString)

aesonDeclarations :: TypeKind -> ClientTypeDefinition -> Q [Dec]
aesonDeclarations KindEnum clientDef = do
    a <- deriveIfNotDefined deriveFromJSON ''FromJSON clientDef
    b <- deriveIfNotDefined deriveToJSON ''ToJSON clientDef
    pure (a <> b)
aesonDeclarations KindScalar clientDef = deriveScalarJSON clientDef
aesonDeclarations kind clientDef
    | isResolverType kind = deriveIfNotDefined deriveFromJSON ''FromJSON clientDef
    | otherwise = deriveIfNotDefined deriveToJSON ''ToJSON clientDef

deriveIfNotDefined :: (ClientTypeDefinition -> Q Dec) -> Name -> ClientTypeDefinition -> Q [Dec]
deriveIfNotDefined derivation typeClass clientDef = do
    exists <- isTypeDeclared clientDef
    if exists
        then do
            has <- hasInstance typeClass clientDef
            if has
                then pure []
                else mkDerivation
        else mkDerivation
  where
    mkDerivation :: Q [Dec]
    mkDerivation = do
        derived <- derivation clientDef
        pure [derived]


failure :: GQLError -> Q a
failure = fail . show

deriveScalarJSON :: ClientTypeDefinition -> Q [Dec]
deriveScalarJSON clientDef = do
    a <- deriveIfNotDefined deriveScalarFromJSON ''FromJSON clientDef
    b <- deriveIfNotDefined deriveScalarToJSON ''ToJSON clientDef
    pure (a <> b)

deriveScalarFromJSON :: ClientTypeDefinition -> DecQ
deriveScalarFromJSON ClientTypeDefinition {clientTypeName} =
  defineFromJSON clientTypeName [|scalarFromJSON|]

deriveScalarToJSON :: ClientTypeDefinition -> DecQ
deriveScalarToJSON
  ClientTypeDefinition
    { clientTypeName = TypeNameTH {typename}
    } = instanceD (cxt []) typeDef body
    where
      typeDef = applyCons ''ToJSON [typename]
      body = [funDSimple 'toJSON [] [|scalarToJSON|]]

-- FromJSON
deriveFromJSON :: ClientTypeDefinition -> DecQ
deriveFromJSON ClientTypeDefinition {clientCons = [], clientTypeName} =
  failure
    $ internal
    $ "Type "
      <> msg (typename clientTypeName)
      <> " Should Have at least one Constructor"
deriveFromJSON ClientTypeDefinition {clientTypeName, clientCons, clientKind = KindEnum}
  =
    defineFromJSON clientTypeName $
      aesonFromJSONEnumBody clientTypeName clientCons
deriveFromJSON
  ClientTypeDefinition
    { clientTypeName = clientTypeName@TypeNameTH {namespace},
      clientCons = [cons]
    } =
    defineFromJSON clientTypeName $
      aesonObject namespace cons
deriveFromJSON typeD@ClientTypeDefinition {clientTypeName}
      =
    defineFromJSON clientTypeName $
      aesonUnionObject typeD

aesonObject :: [FieldName] -> ClientConstructorDefinition -> ExpQ
aesonObject tNamespace con@ClientConstructorDefinition {cName} =
  withBody
    <$> aesonObjectBody tNamespace con
  where
    withBody body =
      AppE
        (AppE (VarE 'withObject) name)
        (LamE [v'] body)
    name :: Exp
    name = toString (camelCaseTypeName tNamespace cName)

aesonObjectBody :: [FieldName] -> ClientConstructorDefinition -> ExpQ
aesonObjectBody namespace ClientConstructorDefinition {cName, cFields} =
  decodeObjectE
    entry
    (camelCaseTypeName namespace cName)
    cFields

entry :: Bool -> Name
entry nullable
  | nullable = '(.:?)
  | otherwise = '(.:)

aesonUnionObject :: ClientTypeDefinition -> ExpQ
aesonUnionObject
  ClientTypeDefinition
    { clientCons,
      clientTypeName = TypeNameTH {namespace, typename}
    } =
    appE [|takeValueType|] $
      matchWith elseCondition f clientCons
    where
      elseCondition =
        (tupP [_', v'],)
          . aesonObjectBody
            namespace
          <$> find ((typename ==) . cName) clientCons
      f cons@ClientConstructorDefinition {cName, cFields} =
        ( tupP [toString cName, if null cFields then _' else v'],
          aesonObjectBody namespace cons
        )

takeValueType :: ((String, Object) -> Parser a) -> Value -> Parser a
takeValueType f (Object hMap) = case lookup "__typename" hMap of
  Nothing -> fail "key \"__typename\" not found on object"
  Just (String x) -> pure (T.unpack x, hMap) >>= f
  Just val ->
    fail $ "key \"__typename\" should be string but found: " <> show val
takeValueType _ _ = fail "expected Object"

namespaced :: TypeNameTH -> TypeName
namespaced TypeNameTH {namespace, typename} =
  camelCaseTypeName namespace typename

defineFromJSON :: TypeNameTH -> ExpQ -> DecQ
defineFromJSON name expr = instanceD (cxt []) typeDef body
  where
    typeDef = applyCons ''FromJSON [namespaced name]
    body = [funDSimple 'parseJSON [] expr]

aesonFromJSONEnumBody :: TypeNameTH -> [ClientConstructorDefinition] -> ExpQ
aesonFromJSONEnumBody TypeNameTH {typename} = matchWith (Just (v', failExp)) f
  where
    f :: ClientConstructorDefinition -> (PatQ, ExpQ)
    f ClientConstructorDefinition {cName} =
      ( toString cName,
        appE [|pure|] $ toCon $ camelCaseTypeName [typename] cName
      )

aesonToJSONEnumBody :: TypeNameTH -> [ClientConstructorDefinition] -> ExpQ
aesonToJSONEnumBody TypeNameTH {typename} = matchWith Nothing f
  where
    f :: ClientConstructorDefinition -> (PatQ, ExpQ)
    f ClientConstructorDefinition {cName} =
      ( conP (toName $ camelCaseTypeName [typename] cName) [],
        toString cName
      )

-- ToJSON
deriveToJSON :: ClientTypeDefinition -> DecQ
deriveToJSON
  ClientTypeDefinition
    { clientCons = []
    } =
    fail "Type Should Have at least one Constructor"
deriveToJSON
  ClientTypeDefinition
    { clientTypeName = clientTypeName@TypeNameTH {typename},
      clientCons,
      clientKind = KindEnum
    } = instanceD (cxt []) typeDef body
    where
      typeDef = applyCons ''ToJSON [typename]
      body = [funDSimple 'toJSON [] (aesonToJSONEnumBody clientTypeName clientCons)]
deriveToJSON
  ClientTypeDefinition
    { clientTypeName = TypeNameTH {typename},
      clientCons = [ClientConstructorDefinition {cFields}]
    } =
    instanceD (cxt []) appHead methods
    where
      appHead = applyCons ''ToJSON [typename]
      ------------------------------------------------------------------
      -- defines: toJSON (User field1 field2 ...)= object ["name" .= name, "age" .= age, ...]
      methods = [funDSimple 'toJSON args body]
        where
          args = [destructRecord typename cFields]
          body =
            pure $
              AppE
                (VarE 'omitNulls)
                (mkFieldsE typename '(.=) cFields)
deriveToJSON _ = fail "Input Unions are not yet supported"

omitNulls :: [Pair] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull _ = True