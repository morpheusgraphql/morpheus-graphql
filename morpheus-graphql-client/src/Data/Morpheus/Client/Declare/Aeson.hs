{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Client.Declare.Aeson
  ( aesonDeclarations,
  )
where

--
-- MORPHEUS
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as H
  ( lookup,
  )
import Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Internal.TH
  ( applyCons,
    decodeObjectE,
    destructRecord,
    funDSimple,
    matchWith,
    mkFieldsE,
    toConE,
    toName,
    toString,
    v',
  )
import Data.Morpheus.Internal.Utils
  ( nameSpaceType,
  )
import Data.Morpheus.Types.GQLScalar
  ( scalarFromJSON,
    scalarToJSON,
  )
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    FieldName,
    Message,
    TypeKind (..),
    TypeName (..),
    isEnum,
    isOutputObject,
    msg,
    toFieldName,
  )
import Data.Semigroup ((<>))
import Data.Text
  ( unpack,
  )
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
    varE,
  )

aesonDeclarations :: TypeKind -> [ClientTypeDefinition -> DecQ]
aesonDeclarations KindEnum = [deriveFromJSON, deriveToJSON]
aesonDeclarations KindScalar = deriveScalarJSON
aesonDeclarations kind
  | isOutputObject kind || kind == KindUnion = [deriveFromJSON]
  | otherwise = [deriveToJSON]

failure :: Message -> Q a
failure = fail . show

deriveScalarJSON :: [ClientTypeDefinition -> DecQ]
deriveScalarJSON = [deriveScalarFromJSON, deriveScalarToJSON]

deriveScalarFromJSON :: ClientTypeDefinition -> DecQ
deriveScalarFromJSON ClientTypeDefinition {clientTypeName} =
  defineFromJSON clientTypeName (varE 'scalarFromJSON)

deriveScalarToJSON :: ClientTypeDefinition -> DecQ
deriveScalarToJSON
  ClientTypeDefinition
    { clientTypeName = TypeNameTH {typename}
    } = instanceD (cxt []) typeDef body
    where
      typeDef = applyCons ''ToJSON [typename]
      body = [funDSimple 'toJSON [] (varE 'scalarToJSON)]

-- FromJSON
deriveFromJSON :: ClientTypeDefinition -> DecQ
deriveFromJSON ClientTypeDefinition {clientCons = [], clientTypeName} =
  failure $
    "Type "
      <> msg (typename clientTypeName)
      <> " Should Have at least one Constructor"
deriveFromJSON
  ClientTypeDefinition
    { clientTypeName = clientTypeName@TypeNameTH {namespace},
      clientCons = [cons]
    } =
    defineFromJSON clientTypeName $
      aesonObject namespace cons
deriveFromJSON typeD@ClientTypeDefinition {clientTypeName, clientCons}
  | isEnum clientCons =
    defineFromJSON clientTypeName $
      aesonFromJSONEnumBody clientTypeName clientCons
  | otherwise =
    defineFromJSON clientTypeName $
      aesonUnionObject typeD

aesonObject :: [FieldName] -> ConsD cat -> ExpQ
aesonObject tNamespace con@ConsD {cName} = do
  body <- aesonObjectBody tNamespace con
  pure $
    AppE
      (AppE (VarE 'withObject) name)
      (LamE [v'] body)
  where
    name :: Exp
    name = toString (nameSpaceType tNamespace cName)

aesonObjectBody :: [FieldName] -> ConsD cat -> ExpQ
aesonObjectBody namespace ConsD {cName, cFields} =
  decodeObjectE
    entry
    (nameSpaceType namespace cName)
    cFields

entry :: Bool -> Name
entry nullable
  | nullable = '(.:?)
  | otherwise = '(.:)

aesonUnionObject :: ClientTypeDefinition -> ExpQ
aesonUnionObject
  ClientTypeDefinition
    { clientCons,
      clientTypeName = TypeNameTH {namespace}
    } =
    appE (varE 'takeValueType) $
      matchWith f clientCons
    where
      f cons@ConsD {cName} =
        ( tupP [toString cName, v'],
          aesonObjectBody namespace cons
        )

takeValueType :: ((String, Object) -> Parser a) -> Value -> Parser a
takeValueType f (Object hMap) = case H.lookup "__typename" hMap of
  Nothing -> fail "key \"__typename\" not found on object"
  Just (String x) -> pure (unpack x, hMap) >>= f
  Just val ->
    fail $ "key \"__typename\" should be string but found: " <> show val
takeValueType _ _ = fail "expected Object"

namespaced :: TypeNameTH -> TypeName
namespaced TypeNameTH {namespace, typename} =
  nameSpaceType namespace typename

defineFromJSON :: TypeNameTH -> ExpQ -> DecQ
defineFromJSON name expr = instanceD (cxt []) typeDef body
  where
    typeDef = applyCons ''FromJSON [namespaced name]
    body = [funDSimple 'parseJSON [] expr]

aesonFromJSONEnumBody :: TypeNameTH -> [ConsD cat] -> ExpQ
aesonFromJSONEnumBody TypeNameTH {typename} = matchWith f
  where
    f :: ConsD cat -> (PatQ, ExpQ)
    f ConsD {cName} =
      ( toString cName,
        appE (varE 'pure) $ toConE $ nameSpaceType [toFieldName typename] cName
      )

aesonToJSONEnumBody :: TypeNameTH -> [ConsD cat] -> ExpQ
aesonToJSONEnumBody TypeNameTH {typename} = matchWith f
  where
    f :: ConsD cat -> (PatQ, ExpQ)
    f ConsD {cName} =
      ( conP (toName $ nameSpaceType [toFieldName typename] cName) [],
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
    { clientTypeName = TypeNameTH {typename},
      clientCons = [ConsD {cFields}]
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
                (VarE 'object)
                (mkFieldsE '(.=) cFields)
deriveToJSON
  ClientTypeDefinition
    { clientTypeName = clientTypeName@TypeNameTH {typename},
      clientCons
    }
    | isEnum clientCons = instanceD (cxt []) typeDef body
    | otherwise = fail "Input Unions are not yet supported"
    where
      typeDef = applyCons ''ToJSON [typename]
      body = [funDSimple 'toJSON [] (aesonToJSONEnumBody clientTypeName clientCons)]
