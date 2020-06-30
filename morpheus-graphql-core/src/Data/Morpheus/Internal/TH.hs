{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Internal.TH
  ( tyConArgs,
    apply,
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
    toVarE,
    toVarT,
    nameConType,
    nameVarP,
    declareTypeRef,
    nameSpaceField,
    nameSpaceType,
    m_,
    m',
    isEnum,
    mkFieldsE,
    toConE,
    toName,
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
m' = toVar m_

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
    decType (Just par) = AppT typeName (toVar par)
    decType _ = typeName

tyConArgs :: TypeKind -> [TypeName]
tyConArgs kindD
  | isOutputObject kindD || kindD == KindUnion = [m_]
  | otherwise = []

cons :: ToTH a b => [a] -> [b]
cons = map toCon

vars :: ToTH a b => [a] -> [b]
vars = map toVar

class ToName a where
  toName :: a -> Name

instance ToName Name where
  toName = id

instance ToName TypeName where
  toName = mkName . unpack . readTypeName

instance ToName FieldName where
  toName = mkName . unpack . readName . convertToHaskellName

class ToTH a b where
  toCon :: a -> b
  toVar :: a -> b

instance ToTH a b => ToTH a (Q b) where
  toCon = pure . toCon
  toVar = pure . toVar

instance (ToName a) => ToTH a Type where
  toCon = ConT . toName
  toVar = VarT . toName

instance (ToName a) => ToTH a Exp where
  toCon = ConE . toName
  toVar = VarE . toName

class Apply a where
  apply :: ToTH i a => i -> [a] -> a

instance Apply TypeQ where
  apply = foldl appT . toCon

instance Apply Type where
  apply = foldl AppT . toCon

instance Apply Exp where
  apply = foldl AppE . toCon

instance Apply ExpQ where
  apply = foldl appE . toCon

typeT :: Name -> [TypeName] -> Q Type
typeT name li = apply name (vars li)

instanceHeadT :: Name -> TypeName -> [TypeName] -> Q Type
instanceHeadT cName iType tArgs = apply cName [apply iType (vars tArgs)]

instanceProxyFunD :: (Name, ExpQ) -> DecQ
instanceProxyFunD (name, body) = instanceFunD name ["_"] body

simpleFunD :: Name -> [PatQ] -> ExpQ -> DecQ
simpleFunD name args body = funD name [clause args (normalB body) []]

instanceFunD :: Name -> [TypeName] -> ExpQ -> Q Dec
instanceFunD name args = simpleFunD name (map (varP . toName) args)

instanceHeadMultiT :: Name -> Q Type -> [Q Type] -> Q Type
instanceHeadMultiT className iType li = apply className (iType : li)

-- |
-- input:
-- >>>
-- destructRecord "User" ["name","id"]
-- >>>
--
-- expression:
-- >>>
-- (User name id)
-- >>>
destructRecord :: TypeName -> [FieldDefinition cat] -> PatQ
destructRecord conName fields = conP (toName conName) (map (varP . toName) names)
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

nameConT :: TypeName -> Q Type
nameConT = conT . toName

toVarT :: ToTH a TypeQ => a -> TypeQ
toVarT = toVar

nameConType :: TypeName -> Type
nameConType = ConT . toName

toVarE :: ToTH a Exp => a -> ExpQ
toVarE = toVar

toConE :: ToTH a Exp => a -> ExpQ
toConE = toCon

nameVarP :: FieldName -> PatQ
nameVarP = varP . toName

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
    (toVar fieldName)

fieldNameStringE :: FieldName -> Exp
fieldNameStringE (FieldName x) = LitE $ StringL (unpack x)
