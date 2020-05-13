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
    makeName,
    nameLitP,
    nameStringL,
    nameConT,
    nameVarT,
  )
where

import Data.Maybe (maybe)
-- MORPHEUS
import Data.Morpheus.Internal.Utils
  ( isEnum,
    nameSpaceType,
    nameSpaceWith,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    ConsD (..),
    DataTypeKind (..),
    DataTypeKind (..),
    FieldDefinition (..),
    FieldName,
    Key,
    TypeD (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    isOutputObject,
    isSubscription,
    readName,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( UnSubResolver,
  )
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Language.Haskell.TH

type Arrow = (->)

m' :: Type
m' = VarT $ makeName m_

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
    typeName = ConT (makeName typeConName)
    --------------------------------------------
    decType _
      | isSub =
        AppT typeName (AppT (ConT ''UnSubResolver) m')
    decType (Just par) = AppT typeName (VarT $ makeName par)
    decType _ = typeName

tyConArgs :: DataTypeKind -> [TypeName]
tyConArgs kindD
  | isOutputObject kindD || kindD == KindUnion = [m_]
  | otherwise = []

data Scope = CLIENT | SERVER
  deriving (Eq)

declareType :: Scope -> Bool -> Maybe DataTypeKind -> [Name] -> TypeD -> Dec
declareType scope namespace kindD derivingList TypeD {tName, tCons, tNamespace} =
  DataD [] (genName tName) tVars Nothing cons $
    map derive (''Generic : derivingList)
  where
    genName = makeName . nameSpaceType tNamespace
    tVars = maybe [] (declareTyVar . tyConArgs) kindD
      where
        declareTyVar = map (PlainTV . makeName)
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
        declareField FieldDefinition {fieldName, fieldArgs, fieldType} =
          (fName, defBang, fiType)
          where
            fName
              | namespace = makeName (nameSpaceWith tName fieldName)
              | otherwise = makeName fieldName
            fiType = genFieldT fieldArgs
              where
                ---------------------------
                genFieldT ArgumentsDefinition {argumentsTypename = Just argsTypename} =
                  AppT
                    (AppT arrowType argType)
                    (AppT m' result)
                  where
                    argType = ConT $ makeName argsTypename
                    arrowType = ConT ''Arrow
                genFieldT _
                  | (isOutputObject <$> kindD) == Just True = AppT m' result
                  | otherwise = result
                ------------------------------------------------
                result = declareTypeRef (maybe False isSubscription kindD) fieldType

makeName :: Key -> Name
makeName = mkName . unpack . readName

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)

applyT :: Name -> [Q Type] -> Q Type
applyT name = foldl appT (conT name)

typeT :: Name -> [TypeName] -> Q Type
typeT name li = applyT name (map (varT . makeName) li)

instanceHeadT :: Name -> TypeName -> [TypeName] -> Q Type
instanceHeadT cName iType tArgs = applyT cName [applyT (makeName iType) (map (varT . makeName) tArgs)]

instanceProxyFunD :: (Name, ExpQ) -> DecQ
instanceProxyFunD (name, body) = instanceFunD name ["_"] body

instanceFunD :: Name -> [FieldName] -> ExpQ -> Q Dec
instanceFunD name args body = funD name [clause (map (varP . makeName) args) (normalB body) []]

instanceHeadMultiT :: Name -> Q Type -> [Q Type] -> Q Type
instanceHeadMultiT className iType li = applyT className (iType : li)

-- "User" -> ["name","id"] -> (User name id)
destructRecord :: TypeName -> [FieldName] -> PatQ
destructRecord conName fields = conP (makeName conName) (map (varP . makeName) fields)

typeInstanceDec :: Name -> Type -> Type -> Dec

nameLitP :: TypeName -> PatQ
nameLitP = litP . nameStringL

nameStringL :: TypeName -> Lit
nameStringL = stringL . unpack . readName

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
nameConT = conT . makeName

nameVarT :: TypeName -> Q Type
nameVarT = varT . makeName
