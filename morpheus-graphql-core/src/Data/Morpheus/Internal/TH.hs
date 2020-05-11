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
    Key,
    TypeD (..),
    TypeRef (..),
    TypeWrapper (..),
    isOutputObject,
    isSubscription,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( UnSubResolver,
  )
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Language.Haskell.TH

type Arrow = (->)

m_ :: Key
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
    typeName = ConT (mkName $ unpack typeConName)
    --------------------------------------------
    decType _
      | isSub =
        AppT typeName (AppT (ConT ''UnSubResolver) (VarT $ mkName $ unpack m_))
    decType (Just par) = AppT typeName (VarT $ mkName $ unpack par)
    decType _ = typeName

tyConArgs :: DataTypeKind -> [Key]
tyConArgs kindD
  | isOutputObject kindD || kindD == KindUnion = [m_]
  | otherwise = []

data Scope = CLIENT | SERVER
  deriving (Eq)

-- declareType
declareType :: Scope -> Bool -> Maybe DataTypeKind -> [Name] -> TypeD -> Dec
declareType scope namespace kindD derivingList TypeD {tName, tCons, tNamespace} =
  DataD [] (genName tName) tVars Nothing cons $
    map derive (''Generic : derivingList)
  where
    genName = mkName . unpack . nameSpaceType tNamespace
    tVars = maybe [] (declareTyVar . tyConArgs) kindD
      where
        declareTyVar = map (PlainTV . mkName . unpack)
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
              | namespace = mkName $ unpack (nameSpaceWith tName fieldName)
              | otherwise = mkName (unpack fieldName)
            fiType = genFieldT fieldArgs
              where
                monadVar = VarT $ mkName $ unpack m_
                ---------------------------
                genFieldT ArgumentsDefinition {argumentsTypename = Just argsTypename} =
                  AppT
                    (AppT arrowType argType)
                    (AppT monadVar result)
                  where
                    argType = ConT $ mkName (unpack argsTypename)
                    arrowType = ConT ''Arrow
                genFieldT _
                  | (isOutputObject <$> kindD) == Just True = AppT monadVar result
                  | otherwise = result
                ------------------------------------------------
                result = declareTypeRef (maybe False isSubscription kindD) fieldType

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)

applyT :: Name -> [Q Type] -> Q Type
applyT name = foldl appT (conT name)

typeT :: Name -> [Text] -> Q Type
typeT name li = applyT name (map (varT . mkName . unpack) li)

instanceHeadT :: Name -> Text -> [Text] -> Q Type
instanceHeadT cName iType tArgs = applyT cName [applyT (mkName $ unpack iType) (map (varT . mkName . unpack) tArgs)]

instanceProxyFunD :: (Name, ExpQ) -> DecQ
instanceProxyFunD (name, body) = instanceFunD name ["_"] body

instanceFunD :: Name -> [Text] -> ExpQ -> Q Dec
instanceFunD name args body = funD name [clause (map (varP . mkName . unpack) args) (normalB body) []]

instanceHeadMultiT :: Name -> Q Type -> [Q Type] -> Q Type
instanceHeadMultiT className iType li = applyT className (iType : li)

-- "User" -> ["name","id"] -> (User name id)
destructRecord :: Text -> [Text] -> PatQ
destructRecord conName fields = conP (mkName $ unpack conName) (map (varP . mkName . unpack) fields)

typeInstanceDec :: Name -> Type -> Type -> Dec

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
