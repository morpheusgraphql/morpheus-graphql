{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Internal.TH
  ( tyConArgs,
    apply,
    applyT,
    typeT,
    instanceHeadT,
    instanceProxyFunD,
    instanceFunD,
    simpleFunD,
    instanceHeadMultiT,
    destructRecord,
    typeInstanceDec,
    infoTyVars,
    decArgs,
    nameLitP,
    nameStringL,
    nameConT,
    nameVarE,
    nameVarT,
    nameConType,
    nameConE,
    nameVarP,
    mkTypeName,
    mkFieldName,
    declareTypeRef,
    nameSpaceField,
    nameSpaceType,
    m_,
    m',
    isEnum,
    mkFieldsE,
  )
where

-- MORPHEUS
import Data.Morpheus.Internal.Utils
  ( nameSpaceField,
    nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldDefinition (..),
    FieldName (..),
    TypeKind (..),
    TypeKind (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    convertToHaskellName,
    isEnum,
    isOutputObject,
    readName,
  )
import Data.Text (unpack)
import Language.Haskell.TH

m' :: Type
m' = VarT $ mkTypeName m_

m_ :: TypeName
m_ = "m"

declareTypeRef :: TypeRef -> Type
declareTypeRef TypeRef {typeConName, typeWrappers, typeArgs} =
  wrappedT
    typeWrappers
  where
    wrappedT :: [TypeWrapper] -> Type
    wrappedT (TypeList : xs) = AppT (ConT ''[]) $ wrappedT xs
    wrappedT (TypeMaybe : xs) = AppT (ConT ''Maybe) $ wrappedT xs
    wrappedT [] = decType typeArgs
    ------------------------------------------------------
    typeName = nameConType typeConName
    --------------------------------------------
    decType (Just par) = AppT typeName (VarT $ mkTypeName par)
    decType _ = typeName

tyConArgs :: TypeKind -> [TypeName]
tyConArgs kindD
  | isOutputObject kindD || kindD == KindUnion = [m_]
  | otherwise = []

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)

applyT :: Name -> [Q Type] -> Q Type
applyT name = foldl appT (conT name)

typeT :: Name -> [TypeName] -> Q Type
typeT name li = applyT name (map (varT . mkTypeName) li)

instanceHeadT :: Name -> TypeName -> [TypeName] -> Q Type
instanceHeadT cName iType tArgs = applyT cName [applyT (mkTypeName iType) (map (varT . mkTypeName) tArgs)]

instanceProxyFunD :: (Name, ExpQ) -> DecQ
instanceProxyFunD (name, body) = instanceFunD name ["_"] body

simpleFunD :: Name -> [PatQ] -> ExpQ -> DecQ
simpleFunD name args body = funD name [clause args (normalB body) []]

instanceFunD :: Name -> [TypeName] -> ExpQ -> Q Dec
instanceFunD name args = simpleFunD name (map (varP . mkTypeName) args)

instanceHeadMultiT :: Name -> Q Type -> [Q Type] -> Q Type
instanceHeadMultiT className iType li = applyT className (iType : li)

-- "User" -> ["name","id"] -> (User name id)
destructRecord :: TypeName -> [FieldDefinition cat] -> PatQ
destructRecord conName fields = conP (mkTypeName conName) (map (varP . mkFieldName) names)
  where
    names = map fieldName fields

typeInstanceDec :: Name -> Type -> Type -> Dec

nameLitP :: TypeName -> PatQ
nameLitP = litP . nameStringL

nameStringL :: TypeName -> Lit
nameStringL = stringL . unpack . readTypeName

#if MIN_VERSION_template_haskell(2,15,0)
-- fix breaking changes
typeInstanceDec typeFamily arg res = TySynInstD (TySynEqn Nothing (AppT (ConT typeFamily) arg) res)
#else
--
typeInstanceDec typeFamily arg res = TySynInstD typeFamily (TySynEqn [arg] res)
#endif

infoTyVars :: Info -> [TyVarBndr]
infoTyVars (TyConI x) = decArgs x
infoTyVars _ = []

decArgs :: Dec -> [TyVarBndr]
decArgs (DataD _ _ args _ _ _) = args
decArgs (NewtypeD _ _ args _ _ _) = args
decArgs (TySynD _ args _) = args
decArgs _ = []

mkTypeName :: TypeName -> Name
mkTypeName = mkName . unpack . readTypeName

mkFieldName :: FieldName -> Name
mkFieldName = mkName . unpack . readName . convertToHaskellName

nameConT :: TypeName -> Q Type
nameConT = conT . mkTypeName

nameConType :: TypeName -> Type
nameConType = ConT . mkTypeName

nameVarT :: TypeName -> Q Type
nameVarT = varT . mkTypeName

nameVarE :: FieldName -> ExpQ
nameVarE = varE . mkFieldName

nameConE :: TypeName -> ExpQ
nameConE = conE . mkTypeName

nameVarP :: FieldName -> PatQ
nameVarP = varP . mkFieldName

-- | 'mkFieldsE'
--
--  input :
--  >>>
--       mkFieldsE 'mkValue [FieldDefinition { fieldName = \"field1" ,..} ,..]
--  >>>
--
--  expression :
--  >>>
--    [ mkValue \"field1\" field1,
--    ..
--    ]
-- >>>
mkFieldsE :: Name -> [FieldDefinition cat] -> Exp
mkFieldsE name = ListE . map (mkEntryWith name)

--  input : mkFieldWith 'mkValue (FieldDefinition { fieldName = "field1", ..})
--  expression: mkValue "field1"  field1
mkEntryWith ::
  Name ->
  FieldDefinition cat ->
  Exp
mkEntryWith f FieldDefinition {fieldName} =
  AppE
    (AppE (VarE f) (fieldNameStringE fieldName))
    (varNameE fieldName)

fieldNameStringE :: FieldName -> Exp
fieldNameStringE (FieldName x) = LitE $ StringL (unpack x)

varNameE :: FieldName -> Exp
varNameE = VarE . mkFieldName
