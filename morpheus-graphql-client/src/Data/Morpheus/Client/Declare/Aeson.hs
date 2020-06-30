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
    destructRecord,
    funDSimple,
    mkFieldsE,
    toConE,
    toName,
    toString,
    toVarE,
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
    FieldDefinition (..),
    FieldName,
    Message,
    TypeKind (..),
    TypeName (..),
    isEnum,
    isNullable,
    isOutputObject,
    msg,
    toFieldName,
  )
import Data.Semigroup ((<>))
import Data.Text
  ( unpack,
  )
import Language.Haskell.TH

aesonDeclarations :: TypeKind -> [ClientTypeDefinition -> Q Dec]
aesonDeclarations KindEnum = [deriveFromJSON, deriveToJSON]
aesonDeclarations KindScalar = deriveScalarJSON
aesonDeclarations kind
  | isOutputObject kind || kind == KindUnion = [deriveFromJSON]
  | otherwise = [deriveToJSON]

failure :: Message -> Q a
failure = fail . show

deriveScalarJSON :: [ClientTypeDefinition -> Q Dec]
deriveScalarJSON = [deriveScalarFromJSON, deriveScalarToJSON]

deriveScalarFromJSON :: ClientTypeDefinition -> Q Dec
deriveScalarFromJSON ClientTypeDefinition {clientTypeName} =
  defineFromJSON
    clientTypeName
    (const $ varE 'scalarFromJSON)
    clientTypeName

deriveScalarToJSON :: ClientTypeDefinition -> Q Dec
deriveScalarToJSON
  ClientTypeDefinition
    { clientTypeName = TypeNameTH {typename}
    } =
    let methods = [funD 'toJSON clauses]
        clauses = [clause [] (normalB $ varE 'scalarToJSON) []]
     in instanceD (cxt []) (applyCons ''ToJSON [typename]) methods

-- FromJSON
deriveFromJSON :: ClientTypeDefinition -> Q Dec
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
    defineFromJSON
      clientTypeName
      (aesonObject namespace)
      cons
deriveFromJSON typeD@ClientTypeDefinition {clientTypeName, clientCons}
  | isEnum clientCons =
    defineFromJSON
      clientTypeName
      (aesonFromJSONEnumBody clientTypeName)
      clientCons
  | otherwise =
    defineFromJSON
      clientTypeName
      aesonUnionObject
      typeD

aesonObject :: [FieldName] -> ConsD cat -> ExpQ
aesonObject tNamespace con@ConsD {cName} =
  appE
    [|withObject name|]
    (lamE [v'] (aesonObjectBody tNamespace con))
  where
    name = nameSpaceType tNamespace cName

aesonObjectBody :: [FieldName] -> ConsD cat -> ExpQ
aesonObjectBody namespace ConsD {cName, cFields} = handleFields cFields
  where
    consName = toConE (nameSpaceType namespace cName)
    ----------------------------------------------------------
    handleFields [] =
      failure $
        "Type \""
          <> msg cName
          <> "\" is Empty Object"
    handleFields fields = startExp fields
      where
        ----------------------------------------------------------

        defField field@FieldDefinition {fieldName}
          | isNullable field = [|v .:? fieldName|]
          | otherwise = [|v .: fieldName|]
        --------------------------------------------------------
        startExp fNames =
          uInfixE
            consName
            (varE '(<$>))
            (applyFields fNames)
          where
            applyFields [] = fail "No Empty fields"
            applyFields [x] = defField x
            applyFields (x : xs) =
              uInfixE (defField x) (varE '(<*>)) (applyFields xs)

aesonUnionObject :: ClientTypeDefinition -> ExpQ
aesonUnionObject
  ClientTypeDefinition
    { clientCons,
      clientTypeName = TypeNameTH {namespace}
    } =
    appE
      (varE 'takeValueType)
      (lamCaseE (map buildMatch clientCons <> [elseCaseEXP]))
    where
      buildMatch cons@ConsD {cName} = match objectPattern body []
        where
          objectPattern = tupP [toString cName, v']
          body = normalB $ aesonObjectBody namespace cons

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

defineFromJSON :: TypeNameTH -> (t -> ExpQ) -> t -> Q Dec
defineFromJSON name parseJ cFields = instanceD (cxt []) iHead body
  where
    iHead = applyCons ''FromJSON [namespaced name]
    body = [funDSimple 'parseJSON [] (parseJ cFields)]

aesonFromJSONEnumBody :: TypeNameTH -> [ConsD cat] -> ExpQ
aesonFromJSONEnumBody TypeNameTH {typename} cons = lamCaseE handlers
  where
    handlers = map buildMatch cons <> [elseCaseEXP]
      where
        buildMatch ConsD {cName} = match enumPat body []
          where
            enumPat = toString cName
            body =
              normalB $
                appE
                  (varE 'pure)
                  (toConE $ nameSpaceType [toFieldName typename] cName)

elseCaseEXP :: MatchQ
elseCaseEXP = match v' body []
  where
    body =
      normalB $
        appE
          (toVarE 'fail)
          ( uInfixE
              (appE (varE 'show) v')
              (varE '(<>))
              (stringE " is Not Valid Union Constructor")
          )

aesonToJSONEnumBody :: TypeNameTH -> [ConsD cat] -> ExpQ
aesonToJSONEnumBody TypeNameTH {typename} cons = lamCaseE handlers
  where
    handlers = map buildMatch cons
      where
        buildMatch ConsD {cName} = match enumPat body []
          where
            enumPat = conP (toName $ nameSpaceType [toFieldName typename] cName) []
            body = normalB (toString cName)

-- ToJSON
deriveToJSON :: ClientTypeDefinition -> Q Dec
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
    | isEnum clientCons =
      let methods = [funD 'toJSON clauses]
          clauses =
            [ clause
                []
                (normalB $ aesonToJSONEnumBody clientTypeName clientCons)
                []
            ]
       in instanceD (cxt []) (applyCons ''ToJSON [typename]) methods
    | otherwise =
      fail "Input Unions are not yet supported"
