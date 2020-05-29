{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Morpheus.Internal.TH
  ( declareType,
    tyConArgs,
    Scope (..),
    apply,
    applyT,
    typeT,
    instanceHeadT,
    instanceProxyFunD,
    instanceFunD,
    instanceHeadMultiT,
    destructRecord,
    typeInstanceDec,
    infoTyVars,
    decArgs,
    nameLitP,
    nameStringE,
    nameStringL,
    nameConT,
    nameVarE,
    nameVarT,
    nameConType,
    nameConE,
    nameVarP,
    mkTypeName,
  )
where

import Data.Maybe (maybe)
-- MORPHEUS
import Data.Morpheus.Internal.Utils
  ( nameSpaceField,
    nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    TypeD (..),
    TypeKind (..),
    TypeKind (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    convertToHaskellName,
    isEnum,
    isOutputObject,
    isSubscription,
    readName,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( UnSubResolver,
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import GHC.Generics (Generic)
import Language.Haskell.TH

type Arrow = (->)

m' :: Type
m' = VarT $ mkTypeName m_

m_ :: TypeName
m_ = "m"

declareTypeRef :: Bool -> TypeRef -> Type
declareTypeRef isSub TypeRef {typeConName, typeWrappers, typeArgs} =
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
    decType _
      | isSub =
        AppT typeName (AppT (ConT ''UnSubResolver) m')
    decType (Just par) = AppT typeName (VarT $ mkTypeName par)
    decType _ = typeName

tyConArgs :: TypeKind -> [TypeName]
tyConArgs kindD
  | isOutputObject kindD || kindD == KindUnion = [m_]
  | otherwise = []

data Scope = CLIENT | SERVER
  deriving (Eq)

declareType :: Scope -> Bool -> Maybe TypeKind -> [Name] -> TypeD cat -> Dec
declareType scope namespace kindD derivingList TypeD {tName, tCons, tNamespace} =
  DataD [] (genName tName) tVars Nothing cons $
    map derive (''Generic : derivingList)
  where
    genName = mkTypeName . nameSpaceType tNamespace
    tVars = maybe [] (declareTyVar . tyConArgs) kindD
      where
        declareTyVar = map (PlainTV . mkTypeName)
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT className]
    cons
      | scope == CLIENT && isEnum tCons = map consE tCons
      | otherwise = map consR tCons
    consE ConsD {cName} = NormalC (genName $ tName <> cName) []
    consR ConsD {cName, cFields} =
      RecC
        (genName cName)
        (map declareField cFields)
      where
        declareField FieldDefinition {fieldName, fieldContent, fieldType} =
          (mkFieldName fName, defBang, fiType)
          where
            fName
              | namespace = nameSpaceField tName fieldName
              | otherwise = fieldName
            fiType = genFieldT result kindD fieldContent
              where
                ---------------------------
                result = declareTypeRef (maybe False isSubscription kindD) fieldType

------------------------------------------------
genFieldT :: Type -> Maybe TypeKind -> FieldContent cat -> Type
genFieldT result _ (FieldArgs ArgumentsDefinition {argumentsTypename = Just argsTypename}) =
  AppT
    (AppT arrowType argType)
    (AppT m' result)
  where
    argType = ConT $ mkTypeName argsTypename
    arrowType = ConT ''Arrow
genFieldT result kind _
  | (isOutputObject <$> kind) == Just True = AppT m' result
  | otherwise = result

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

instanceFunD :: Name -> [TypeName] -> ExpQ -> Q Dec
instanceFunD name args body = funD name [clause (map (varP . mkTypeName) args) (normalB body) []]

instanceHeadMultiT :: Name -> Q Type -> [Q Type] -> Q Type
instanceHeadMultiT className iType li = applyT className (iType : li)

-- "User" -> ["name","id"] -> (User name id)
destructRecord :: TypeName -> [FieldName] -> PatQ
destructRecord conName fields = conP (mkTypeName conName) (map (varP . mkFieldName) fields)

typeInstanceDec :: Name -> Type -> Type -> Dec

nameLitP :: TypeName -> PatQ
nameLitP = litP . nameStringL

nameStringL :: TypeName -> Lit
nameStringL = stringL . unpack . readTypeName

nameStringE :: TypeName -> ExpQ
nameStringE = stringE . (unpack . readTypeName)

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
