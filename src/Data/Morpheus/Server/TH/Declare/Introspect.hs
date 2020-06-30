{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.TH.Declare.Introspect
  ( deriveObjectRep,
    instanceIntrospect,
  )
where

-- MORPHEUS
import Data.Morpheus.Internal.TH
  ( instanceFunD,
    instanceHeadMultiT,
    instanceProxyFunD,
    mkTypeName,
    nameConT,
    nameVarT,
    tyConArgs,
    typeT,
  )
import Data.Morpheus.Internal.Utils
  ( concatUpdates,
  )
import Data.Morpheus.Server.Deriving.Introspect
  ( DeriveTypeContent (..),
    Introspect (..),
    ProxyRep (..),
    deriveCustomInputObjectType,
  )
import Data.Morpheus.Server.Internal.TH.Types (ServerTypeDefinition (..))
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (__typeName, implements),
    TypeUpdater,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    ConsD (..),
    FieldContent (..),
    FieldDefinition (..),
    IN,
    OUT,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    insertType,
    unsafeFromFields,
  )
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Language.Haskell.TH

cat_ :: TypeQ
cat_ = varT (mkName "cat")

instanceIntrospect :: Maybe (TypeDefinition cat) -> Q [Dec]
instanceIntrospect (Just typeDef@TypeDefinition {typeName, typeContent = DataEnum {}}) =
  pure <$> instanceD (cxt []) iHead [defineIntrospect]
  where
    iHead = instanceHeadMultiT ''Introspect cat_ [conT $ mkTypeName typeName]
    defineIntrospect = instanceProxyFunD ('introspect, [|insertType typeDef|])
instanceIntrospect _ = pure []

constraintTypeable :: TypeName -> Q Type
constraintTypeable name = typeT ''Typeable [name]

-- [(FieldDefinition, TypeUpdater)]
deriveObjectRep :: ServerTypeDefinition cat -> Q [Dec]
deriveObjectRep
  ServerTypeDefinition
    { tName,
      tCons = [ConsD {cFields}],
      tKind
    } =
    pure <$> instanceD (cxt constrains) iHead methods
    where
      mainTypeName = typeT (mkTypeName tName) typeArgs
      typeArgs = tyConArgs tKind
      constrains = map constraintTypeable typeArgs
      -----------------------------------------------
      iHead = instanceHeadMultiT ''DeriveTypeContent instCat [conT ''TRUE, mainTypeName]
      instCat
        | tKind == KindInputObject =
          conT ''IN
        | otherwise = conT ''OUT
      methods = [instanceFunD 'deriveTypeContent ["_proxy1", "_proxy2"] body]
        where
          body
            | tKind == KindInputObject =
              [|
                deriveInputObject
                  $(buildFields cFields)
                  $(buildTypes instCat cFields)
                |]
            | otherwise =
              [|
                deriveOutputObject
                  $(proxy)
                  $(buildFields cFields)
                  $(buildTypes instCat cFields)
                |]
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
interfaceTypes = concatUpdates . map snd . implements

buildTypes :: TypeQ -> [FieldDefinition cat] -> ExpQ
buildTypes cat = listE . concatMap (introspectField cat)

introspectField :: TypeQ -> FieldDefinition cat -> [ExpQ]
introspectField cat FieldDefinition {fieldType, fieldContent} =
  [|introspect $(proxyRepT cat fieldType)|] : inputTypes fieldContent
  where
    inputTypes :: Maybe (FieldContent TRUE cat) -> [ExpQ]
    inputTypes (Just (FieldArgs ArgumentsDefinition {argumentsTypename = Just argsTypeName}))
      | argsTypeName /= "()" = [[|deriveCustomInputObjectType (argsTypeName, $(proxyT tAlias))|]]
      where
        tAlias = TypeRef {typeConName = argsTypeName, typeWrappers = [], typeArgs = Nothing}
    inputTypes _ = []

proxyRepT :: TypeQ -> TypeRef -> Q Exp
proxyRepT cat TypeRef {typeConName, typeArgs} = [|(ProxyRep :: ProxyRep $(cat) $(genSig typeArgs))|]
  where
    genSig (Just m) = appT (nameConT typeConName) (nameVarT m)
    genSig _ = nameConT typeConName

proxyT :: TypeRef -> Q Exp
proxyT TypeRef {typeConName, typeArgs} = [|(Proxy :: Proxy $(genSig typeArgs))|]
  where
    genSig (Just m) = appT (nameConT typeConName) (nameVarT m)
    genSig _ = nameConT typeConName

buildFields :: [FieldDefinition cat] -> ExpQ
buildFields = listE . map buildField
  where
    buildField f@FieldDefinition {fieldType} = [|f {fieldType = fieldType {typeConName = __typeName $(proxyT fieldType)}}|]
