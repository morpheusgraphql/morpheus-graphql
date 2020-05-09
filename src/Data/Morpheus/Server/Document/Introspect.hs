{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Document.Introspect
  ( deriveObjectRep,
    instanceIntrospect,
  )
where

import Data.Maybe (maybeToList)
-- MORPHEUS
import Data.Morpheus.Internal.TH
  ( instanceFunD,
    instanceHeadMultiT,
    instanceHeadT,
    instanceProxyFunD,
    tyConArgs,
    typeT,
  )
import Data.Morpheus.Server.Deriving.Introspect (Introspect (..), IntrospectRep (..), TypeScope (..), introspectObjectFields)
import Data.Morpheus.Server.Types.GQLType (GQLType (__typeName), TRUE)
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    ConsD (..),
    DataTypeKind (..),
    FieldDefinition (..),
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeRef (..),
    insertType,
    unsafeFromFields,
    unsafeFromInputFields,
  )
import Data.Proxy (Proxy (..))
import Data.Text (Text, unpack)
import Data.Typeable (Typeable)
import Language.Haskell.TH

instanceIntrospect :: TypeDefinition cat -> Q [Dec]
instanceIntrospect TypeDefinition {typeName, typeContent = DataEnum enumType, ..}
  -- FIXME: dirty fix for introspection
  | typeName `elem` ["__DirectiveLocation", "__TypeKind"] = pure []
  | otherwise = pure <$> instanceD (cxt []) iHead [defineIntrospect]
  where
    -----------------------------------------------
    iHead = instanceHeadT ''Introspect typeName []
    defineIntrospect = instanceProxyFunD ('introspect, body)
      where
        body = [|insertType TypeDefinition {typeContent = DataEnum enumType, ..}|]
instanceIntrospect _ = pure []

-- [(FieldDefinition, TypeUpdater)]
deriveObjectRep :: (TypeD, Maybe DataTypeKind) -> Q [Dec]
deriveObjectRep (TypeD {tName, tCons = [ConsD {cFields}]}, tKind) =
  pure <$> instanceD (cxt constrains) iHead methods
  where
    mainTypeName = typeT (mkName $ unpack tName) typeArgs
    typeArgs = concatMap tyConArgs (maybeToList tKind)
    constrains = map conTypeable typeArgs
      where
        conTypeable name = typeT ''Typeable [name]
    -----------------------------------------------
    iHead = instanceHeadMultiT ''IntrospectRep (conT ''TRUE) [mainTypeName]
    methods = [instanceFunD 'introspectRep ["_proxy1", "_proxy2"] body]
      where
        body
          | tKind == Just KindInputObject || null tKind = [|(DataInputObject $ unsafeFromInputFields $(buildFields cFields), concat $(buildTypes cFields))|]
          | otherwise = [|(DataObject [] $ unsafeFromFields $(buildFields cFields), concat $(buildTypes cFields))|]
deriveObjectRep _ = pure []

buildTypes :: [FieldDefinition] -> ExpQ
buildTypes = listE . concatMap introspectField
  where
    introspectField FieldDefinition {fieldType, fieldArgs} =
      [|[introspect $(proxyT fieldType)]|] : inputTypes fieldArgs
      where
        inputTypes ArgumentsDefinition {argumentsTypename = Just argsTypeName}
          | argsTypeName /= "()" = [[|snd $ introspectObjectFields (Proxy :: Proxy TRUE) (argsTypeName, InputType, $(proxyT tAlias))|]]
          where
            tAlias = TypeRef {typeConName = argsTypeName, typeWrappers = [], typeArgs = Nothing}
        inputTypes _ = []

conTX :: Text -> Q Type
conTX = conT . mkName . unpack

varTX :: Text -> Q Type
varTX = varT . mkName . unpack

proxyT :: TypeRef -> Q Exp
proxyT TypeRef {typeConName, typeArgs} = [|(Proxy :: Proxy $(genSig typeArgs))|]
  where
    genSig (Just m) = appT (conTX typeConName) (varTX m)
    genSig _ = conTX typeConName

buildFields :: [FieldDefinition] -> ExpQ
buildFields = listE . map buildField
  where
    buildField f@FieldDefinition {fieldType} = [|f {fieldType = fieldType {typeConName = __typeName $(proxyT fieldType)}}|]
