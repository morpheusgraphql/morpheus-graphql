{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Introspect
  ( deriveObjectRep , instanceIntrospect
  ) where

import Data.Maybe(maybeToList)
import           Data.Proxy                                (Proxy (..))
import           Data.Text                                 (unpack,Text)
import           Data.Typeable                             (Typeable)
import           Language.Haskell.TH  

-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Declare  (tyConArgs)
import           Data.Morpheus.Execution.Server.Introspect (Introspect (..), objectFields, IntrospectRep (..),TypeScope(..))
import           Data.Morpheus.Types.GQLType               (GQLType (__typeName), TRUE)
import           Data.Morpheus.Types.Internal.AST          (ConsD (..), TypeD (..), Key, DataType(..), DataTypeContent(..), DataField (..),insertType,DataTypeKind(..), TypeRef (..))
import           Data.Morpheus.Types.Internal.TH           (instanceFunD, instanceProxyFunD,instanceHeadT, instanceHeadMultiT, typeT)


instanceIntrospect :: (Key,DataType) -> Q [Dec]
-- FIXME: dirty fix for introspection
instanceIntrospect ("__DirectiveLocation",_) = pure []
instanceIntrospect ("__TypeKind",_) = pure []
instanceIntrospect (name, DataType {
      typeContent = DataEnum enumType,
      typeName
      , typeMeta
      , typeFingerprint
    }) = pure <$> instanceD (cxt []) iHead [defineIntrospect]
  where
    -----------------------------------------------
    iHead = instanceHeadT ''Introspect name []
    defineIntrospect = instanceProxyFunD ('introspect,body)
      where
        body =[| insertType (name,DataType {
          typeName
          ,typeMeta
          , typeFingerprint
          ,typeContent = DataEnum enumType
        }) |]
instanceIntrospect _ = pure []

-- [((Text, DataField), TypeUpdater)]
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
          | tKind == Just KindInputObject || null tKind  = [| (DataInputObject  $(buildFields cFields), concat $(buildTypes cFields))|]
          | otherwise  =  [| (DataObject $(buildFields cFields), concat $(buildTypes cFields))|]
deriveObjectRep _ = pure []
    

buildTypes :: [DataField] -> ExpQ
buildTypes = listE . concatMap introspectField
  where
    introspectField DataField {fieldType, fieldArgsType} =
      [|[introspect $(proxyT fieldType)]|] : inputTypes fieldArgsType
      where
        inputTypes (Just argsTypeName)
          | argsTypeName /= "()" = [[|snd $ objectFields (Proxy :: Proxy TRUE) (argsTypeName, InputType,$(proxyT tAlias))|]]
          where
            tAlias = TypeRef {typeConName = argsTypeName, typeWrappers = [], typeArgs = Nothing}
        inputTypes _ = []

conTX :: Text -> Q Type    
conTX =  conT . mkName . unpack

varTX :: Text -> Q Type    
varTX =  varT . mkName . unpack

proxyT :: TypeRef -> Q Exp
proxyT TypeRef {typeConName, typeArgs} = [|(Proxy :: Proxy $(genSig typeArgs))|]
  where
    genSig (Just m) = appT (conTX typeConName) (varTX m)
    genSig _        = conTX typeConName

buildFields :: [DataField] -> ExpQ
buildFields = listE . map buildField
  where
    buildField DataField {fieldName, fieldArgs, fieldType = alias@TypeRef {typeArgs, typeWrappers}, fieldMeta} =
      [|( fieldName
        , DataField
            { fieldName
            , fieldArgs = fArgs
            , fieldArgsType = Nothing
            , fieldType = TypeRef {typeConName = __typeName $(proxyT alias), typeArgs = aArgs, typeWrappers}
            , fieldMeta
            })|]
      where
        fArgs = map (\(k, v) -> (unpack k, v)) fieldArgs
        aArgs = unpack <$> typeArgs
