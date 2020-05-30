{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Document.Declare.Introspect
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
    mkTypeName,
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
import Data.Morpheus.Server.Internal.TH.Types (TypeD (..))
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (__typeName, implements),
    TRUE,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD (..),
    FieldContent (..),
    FieldDefinition (..),
    IN,
    OUT,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
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
    iHead = instanceHeadMultiT ''Introspect (varT (mkName "cat")) [conT $ mkTypeName typeName]
    defineIntrospect = instanceProxyFunD ('introspect, body)
      where
        body = [|insertType TypeDefinition {typeContent = DataEnum enumType, ..}|]
instanceIntrospect _ = pure []

-- [(FieldDefinition, TypeUpdater)]
deriveObjectRep :: (TypeD cat, Maybe (TypeDefinition ANY), Maybe TypeKind) -> Q [Dec]
deriveObjectRep (TypeD {tName, tCons = [ConsD {cFields}]}, _, tKind) =
  pure <$> instanceD (cxt constrains) iHead methods
  where
    mainTypeName = typeT (mkTypeName tName) typeArgs
    typeArgs = concatMap tyConArgs (maybeToList tKind)
    constrains = map conTypeable typeArgs
      where
        conTypeable name = typeT ''Typeable [name]
    -----------------------------------------------
    iHead = instanceHeadMultiT ''DeriveTypeContent (varT (mkName "cat")) [conT ''TRUE, mainTypeName]
    methods = [instanceFunD 'deriveTypeContent ["_proxy1", "_proxy2"] body]
      where
        body
          | tKind == Just KindInputObject || null tKind =
            [|
              deriveInputObject
                $(buildFields cFields)
                $(typeUpdates)
              |]
          | otherwise =
            [|
              deriveOutputObject
                $(proxy)
                $(buildFields cFields)
                $(typeUpdates)
              |]
        -------------------------------------------------------------
        typeUpdates = buildTypes cFields
        proxy = [|(Proxy :: Proxy $(mainTypeName))|]
deriveObjectRep _ = pure []

deriveInputObject ::
  [FieldDefinition IN] ->
  [TypeUpdater] ->
  ( TypeContent TRUE IN,
    [TypeUpdater]
  )
deriveInputObject fields typeUpdates =
  (DataInputObject (unsafeFromFields fields), typeUpdates)

deriveOutputObject ::
  (GQLType a) =>
  Proxy a ->
  [FieldDefinition OUT] ->
  [TypeUpdater] ->
  ( TypeContent TRUE OUT,
    [TypeUpdater]
  )
deriveOutputObject proxy fields typeUpdates =
  ( DataObject
      (interfaceNames proxy)
      (unsafeFromFields fields),
    interfaceTypes proxy : typeUpdates
  )

interfaceNames :: GQLType a => Proxy a -> [TypeName]
interfaceNames = map fst . implements

interfaceTypes :: GQLType a => Proxy a -> TypeUpdater
interfaceTypes = flip resolveUpdates . map snd . implements

buildTypes :: [FieldDefinition cat] -> ExpQ
buildTypes = listE . concatMap introspectField

introspectField :: FieldDefinition cat -> [ExpQ]
introspectField FieldDefinition {fieldType, fieldContent} =
  [|introspect $(proxyT fieldType)|] : inputTypes fieldContent
  where
    inputTypes :: FieldContent TRUE cat -> [ExpQ]
    inputTypes (FieldArgs ArgumentsDefinition {argumentsTypename = Just argsTypeName})
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
