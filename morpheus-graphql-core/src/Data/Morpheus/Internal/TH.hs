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
-- MORPHEUS
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Internal.TH
  ( _',
    apply,
    applyCons,
    applyVars,
    decArgs,
    declareTypeRef,
    destructRecord,
    funDProxy,
    funDSimple,
    infoTyVars,
    isEnum,
    m',
    mkFieldsE,
    nameSpaceField,
    nameSpaceType,
    toCon,
    toConE,
    toConT,
    ToName (..),
    toString,
    toVarE,
    toVarT,
    tyConArgs,
    typeInstanceDec,
    v',
    cat',
    _2',
    o',
    e',
    vars,
    decodeObjectE,
    matchWith,
  )
where

import Control.Applicative ((<*>), pure)
import Control.Monad.Fail (fail)
import Data.Foldable (foldl, foldr1)
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Internal.Utils
  ( capitalize,
    nameSpaceField,
    nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldDefinition (..),
    FieldName (..),
    TypeKind (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    convertToHaskellName,
    isEnum,
    isNullable,
    isOutputObject,
    readName,
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Language.Haskell.TH
import Prelude
  ( ($),
    (.),
    (==),
    Bool (..),
    String,
    id,
    map,
    otherwise,
    show,
    (||),
  )

m' :: Type
m' = VarT (mkName "m")

o' :: Type
o' = VarT (mkName "o")

e' :: Type
e' = VarT (mkName "event")

_' :: PatQ
_' = toVar (mkName "_")

_2' :: PatQ
_2' = toVar (mkName "_2")

v' :: ToVar Name a => a
v' = toVar (mkName "v")

cat' :: Type
cat' = VarT (mkName "cat")

declareTypeRef :: TypeRef -> Type
declareTypeRef TypeRef {typeConName, typeWrappers, typeArgs} =
  wrappedT
    typeWrappers
  where
    wrappedT :: [TypeWrapper] -> Type
    wrappedT (TypeList : xs) = AppT (ConT ''[]) $ wrappedT xs
    wrappedT (TypeMaybe : xs) = AppT (ConT ''Maybe) $ wrappedT xs
    wrappedT [] = decType typeArgs
    --------------------------------------------
    decType :: Maybe String -> Type
    decType (Just par) = apply typeConName [toVar par]
    decType _ = toCon typeConName

tyConArgs :: TypeKind -> [String]
tyConArgs kindD
  | isOutputObject kindD || kindD == KindUnion = ["m"]
  | otherwise = []

cons :: ToCon a b => [a] -> [b]
cons = map toCon

vars :: ToVar a b => [a] -> [b]
vars = map toVar

class ToName a where
  toName :: a -> Name

instance ToName String where
  toName = mkName

instance ToName Name where
  toName = id

instance ToName TypeName where
  toName = mkName . unpack . capitalize . readTypeName

instance ToName FieldName where
  toName = mkName . unpack . readName . convertToHaskellName

class ToString a b where
  toString :: a -> b

instance ToString a b => ToString a (Q b) where
  toString = pure . toString

instance ToString TypeName Lit where
  toString = stringL . unpack . readTypeName

instance ToString TypeName Pat where
  toString = LitP . toString

instance ToString FieldName Lit where
  toString (FieldName x) = stringL (unpack x)

instance ToString TypeName Exp where
  toString = LitE . toString

instance ToString FieldName Exp where
  toString = LitE . toString

class ToCon a b where
  toCon :: a -> b

instance ToCon a b => ToCon a (Q b) where
  toCon = pure . toCon

instance (ToName a) => ToCon a Type where
  toCon = ConT . toName

instance (ToName a) => ToCon a Exp where
  toCon = ConE . toName

class ToVar a b where
  toVar :: a -> b

instance ToVar a b => ToVar a (Q b) where
  toVar = pure . toVar

instance (ToName a) => ToVar a Type where
  toVar = VarT . toName

instance (ToName a) => ToVar a Exp where
  toVar = VarE . toName

instance (ToName a) => ToVar a Pat where
  toVar = VarP . toName

class Apply a where
  apply :: ToCon i a => i -> [a] -> a

instance Apply TypeQ where
  apply = foldl appT . toCon

instance Apply Type where
  apply = foldl AppT . toCon

instance Apply Exp where
  apply = foldl AppE . toCon

instance Apply ExpQ where
  apply = foldl appE . toCon

applyVars ::
  ( ToName con,
    ToName var,
    Apply res,
    ToCon con res,
    ToVar var res
  ) =>
  con ->
  [var] ->
  res
applyVars name li = apply name (vars li)

applyCons :: (ToName con, ToName cons) => con -> [cons] -> Q Type
applyCons name li = apply name (cons li)

funDProxy :: [(Name, ExpQ)] -> [DecQ]
funDProxy = map fun
  where
    fun (name, body) = funDSimple name [_'] body

funDSimple :: Name -> [PatQ] -> ExpQ -> DecQ
funDSimple name args body = funD name [clause args (normalB body) []]

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
destructRecord :: TypeName -> [FieldDefinition cat s] -> PatQ
destructRecord conName fields = conP (toName conName) (vars names)
  where
    names = map fieldName fields

infoTyVars :: Info -> [TyVarBndr]
infoTyVars (TyConI x) = decArgs x
infoTyVars _ = []

decArgs :: Dec -> [TyVarBndr]
decArgs (DataD _ _ args _ _ _) = args
decArgs (NewtypeD _ _ args _ _ _) = args
decArgs (TySynD _ args _) = args
decArgs _ = []

toConT :: ToName a => a -> Q Type
toConT = conT . toName

toVarT :: ToVar a TypeQ => a -> TypeQ
toVarT = toVar

toVarE :: ToVar a Exp => a -> ExpQ
toVarE = toVar

toConE :: ToCon a Exp => a -> ExpQ
toConE = toCon

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
mkFieldsE :: Name -> [FieldDefinition cat s] -> Exp
mkFieldsE name = ListE . map (mkEntryWith name)

--  input : mkFieldWith 'mkValue (FieldDefinition { fieldName = "field1", ..})
--  expression: mkValue "field1"  field1
mkEntryWith ::
  Name ->
  FieldDefinition cat s ->
  Exp
mkEntryWith f FieldDefinition {fieldName} =
  AppE
    (AppE (VarE f) (toString fieldName))
    (toVar fieldName)

decodeObjectE :: (Bool -> Name) -> TypeName -> [FieldDefinition cat s] -> ExpQ
decodeObjectE _ conName [] = appE (varE 'pure) (toCon conName)
decodeObjectE funName conName fields =
  uInfixE
    (toCon conName)
    (varE '(<$>))
    (foldr1 withApplicative $ map (defField funName) fields)

withApplicative :: ExpQ -> ExpQ -> ExpQ
withApplicative x = uInfixE x (varE '(<*>))

matchWith ::
  Bool ->
  (t -> (PatQ, ExpQ)) ->
  [t] ->
  ExpQ
matchWith isClosed f xs = lamCaseE (map buildMatch xs <> fallback)
  where
    fallback
      | isClosed = []
      | otherwise = [elseCaseEXP]
    buildMatch x = match pat (normalB body) []
      where
        (pat, body) = f x

elseCaseEXP :: MatchQ
elseCaseEXP = match v' body []
  where
    body =
      normalB $
        appE
          (toVarE 'fail)
          ( uInfixE
              (appE (varE 'show) v')
              (varE '(<>))
              (stringE " is Not Valid Union Constructor")
          )

defField :: (Bool -> Name) -> FieldDefinition cat s -> ExpQ
defField f field@FieldDefinition {fieldName} = uInfixE v' (varE $ f (isNullable field)) (toString fieldName)

#if MIN_VERSION_template_haskell(2,15,0)
-- fix breaking changes
typeInstanceDec :: Name -> Type -> Type -> Dec
typeInstanceDec typeFamily arg res = TySynInstD (TySynEqn Nothing (AppT (ConT typeFamily) arg) res)
#else
--
typeInstanceDec :: Name -> Type -> Type -> Dec
typeInstanceDec typeFamily arg res = TySynInstD typeFamily (TySynEqn [arg] res)
#endif
