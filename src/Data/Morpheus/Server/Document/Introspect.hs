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
    makeName,
    nameConT,
    nameVarT,
    tyConArgs,
    typeT,
  )
import Data.Morpheus.Server.Deriving.Introspect
  ( DeriveTypeContent (..),
    Introspect (..),
    deriveCustomInputObjectType,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (__typeName, implements),
    TRUE,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD (..),
    DataTypeKind (..),
    FieldDefinition (..),
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    TypeUpdater,
    insertType,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( resolveUpdates,
  )
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Language.Haskell.TH

instanceIntrospect :: TypeDefinition cat -> Q [Dec]
instanceIntrospect TypeDefinition {typeName, typeContent = DataEnum enumType, ..}
  -- FIXME: dirty fix for introspection
  | typeName `elem` ["__DirectiveLocation", "__TypeKind"] = pure []
  | otherwise = pure <$> instanceD (cxt []) iHead [defineIntrospect]
  where
    iHead = instanceHeadT ''Introspect typeName []
    defineIntrospect = instanceProxyFunD ('introspect, body)
      where
        body = [|insertType TypeDefinition {typeContent = DataEnum enumType, ..}|]
instanceIntrospect _ = pure []

-- [(FieldDefinition, TypeUpdater)]
deriveObjectRep :: (TypeD, Maybe (TypeDefinition ANY), Maybe DataTypeKind) -> Q [Dec]
deriveObjectRep (TypeD {tName, tCons = [ConsD {cFields}]}, _, tKind) =
  pure <$> instanceD (cxt constrains) iHead methods
  where
    mainTypeName = typeT (makeName tName) typeArgs
    typeArgs = concatMap tyConArgs (maybeToList tKind)
    constrains = map conTypeable typeArgs
      where
        conTypeable name = typeT ''Typeable [name]
    -----------------------------------------------
    iHead = instanceHeadMultiT ''DeriveTypeContent (conT ''TRUE) [mainTypeName]
    methods = [instanceFunD 'deriveTypeContent ["_proxy1", "_proxy2"] body]
      where
        body
          | tKind == Just KindInputObject || null tKind =
            [|
              ( DataInputObject
                  (unsafeFromFields $(buildFields cFields)),
                $(typeUpdates)
              )
              |]
          | otherwise =
            [|
              ( DataObject
                  (interfaceNames $(proxy))
                  (unsafeFromFields $(buildFields cFields)),
                interfaceTypes $(proxy)
                  : $(typeUpdates)
              )
              |]
        -------------------------------------------------------------
        typeUpdates = buildTypes cFields
        proxy = [|(Proxy :: Proxy $(mainTypeName))|]
deriveObjectRep _ = pure []

interfaceNames :: GQLType a => Proxy a -> [TypeName]
interfaceNames = map fst . implements

interfaceTypes :: GQLType a => Proxy a -> TypeUpdater
interfaceTypes = flip resolveUpdates . map snd . implements

buildTypes :: [FieldDefinition cat] -> ExpQ
buildTypes = listE . concatMap introspectField

introspectField :: FieldDefinition cat -> [ExpQ]
introspectField FieldDefinition {fieldType, fieldArgs} =
  [|introspect $(proxyT fieldType)|] : inputTypes fieldArgs
  where
    inputTypes :: ArgumentsDefinition -> [ExpQ]
    inputTypes ArgumentsDefinition {argumentsTypename = Just argsTypeName}
      | argsTypeName /= "()" = [[|deriveCustomInputObjectType (argsTypeName, $(proxyT tAlias))|]]
      where
        tAlias = TypeRef {typeConName = argsTypeName, typeWrappers = [], typeArgs = Nothing}
    inputTypes _ = []

proxyT :: TypeRef -> Q Exp
proxyT TypeRef {typeConName, typeArgs} = [|(Proxy :: Proxy $(genSig typeArgs))|]
  where
    genSig (Just m) = appT (nameConT typeConName) (nameVarT m)
    genSig _ = nameConT typeConName

buildFields :: [FieldDefinition cat] -> ExpQ
buildFields = listE . map buildField
  where
    buildField f@FieldDefinition {fieldType} = [|f {fieldType = fieldType {typeConName = __typeName $(proxyT fieldType)}}|]
