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
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.TH
  ( _',
    apply,
    applyCons,
    applyVars,
    declareTypeRef,
    funDSimple,
    funDProxy,
    toCon,
    toVar,
    ToName (..),
    toString,
    typeInstanceDec,
    v',
    vars,
    wrappedType,
    PrintExp (..),
    PrintType (..),
    toTypeVars,
    printDerivClause,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( DerivingClass (..),
    TypeValue (..),
  )
import Data.Morpheus.CodeGen.Utils
  ( toHaskellName,
    toHaskellTypeName,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    unpackName,
  )
import qualified Data.Text as T
import Language.Haskell.TH
import Relude hiding
  ( ToString (..),
    Type,
  )

_' :: PatQ
_' = toVar (mkName "_")

v' :: ToVar Name a => a
v' = toVar (mkName "v")

wrappedType :: TypeWrapper -> Type -> Type
wrappedType (TypeList xs nonNull) = withNonNull nonNull . withList . wrappedType xs
wrappedType (BaseType nonNull) = withNonNull nonNull
{-# INLINE wrappedType #-}

declareTypeRef :: (TypeName -> Type) -> TypeRef -> Type
declareTypeRef f TypeRef {typeConName, typeWrappers} =
  wrappedType typeWrappers (f typeConName)
{-# INLINE declareTypeRef #-}

withList :: Type -> Type
withList = AppT (ConT ''[])

withNonNull :: Bool -> Type -> Type
withNonNull True = id
withNonNull False = AppT (ConT ''Maybe)
{-# INLINE withNonNull #-}

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

instance ToName Text where
  toName = toName . T.unpack

instance ToName TypeName where
  toName = toName . toHaskellTypeName

instance ToName FieldName where
  toName = mkName . toHaskellName

class ToString a b where
  toString :: a -> b

instance ToString a b => ToString a (Q b) where
  toString = pure . toString

instance ToString TypeName Lit where
  toString = stringL . T.unpack . unpackName

instance ToString TypeName Pat where
  toString = LitP . toString

instance ToString FieldName Lit where
  toString = stringL . T.unpack . unpackName

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
  apply = foldl' appT . toCon

instance Apply Type where
  apply = foldl' AppT . toCon

instance Apply Exp where
  apply = foldl' AppE . toCon

instance Apply ExpQ where
  apply = foldl' appE . toCon

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

funDSimple :: Name -> [PatQ] -> ExpQ -> DecQ
funDSimple name args body = funD name [clause args (normalB body) []]

funDProxy :: [(Name, ExpQ)] -> [DecQ]
funDProxy = map fun
  where
    fun (name, body) = funDSimple name [_'] body

#if MIN_VERSION_template_haskell(2,15,0)
-- fix breaking changes
typeInstanceDec :: Name -> Type -> Type -> Dec
typeInstanceDec typeFamily arg res = TySynInstD (TySynEqn Nothing (AppT (ConT typeFamily) arg) res)
#else
--
typeInstanceDec :: Name -> Type -> Type -> Dec
typeInstanceDec typeFamily arg res = TySynInstD typeFamily (TySynEqn [arg] res)
#endif

{- ORMOLU_DISABLE -}
toTypeVars :: [Name] -> [TyVarBndr ()]
#if MIN_VERSION_template_haskell(2,17,0)
toTypeVars = map (flip PlainTV ())
#else
toTypeVars map PlainTV
#endif
{- ORMOLU_ENABLE -}
class PrintExp a where
  printExp :: a -> ExpQ

class PrintType a where
  printType :: a -> TypeQ

printFieldExp :: (FieldName, TypeValue) -> Q FieldExp
printFieldExp (fName, fValue) = do
  v <- printExp fValue
  pure (toName fName, v)

instance PrintExp TypeValue where
  printExp (TypeValueObject name xs) = recConE (toName name) (map printFieldExp xs)
  printExp (TypeValueNumber x) = [|x|]
  printExp (TypeValueString x) = litE (stringL (T.unpack x))
  printExp (TypeValueBool _) = [|x|]
  printExp (TypedValueMaybe (Just x)) = appE (conE 'Just) (printExp x)
  printExp (TypedValueMaybe Nothing) = conE 'Nothing
  printExp (TypeValueList xs) = listE $ map printExp xs

genName :: DerivingClass -> Name
genName GENERIC = ''Generic
genName SHOW = ''Show
genName CLASS_EQ = ''Eq

printDerivClause :: [DerivingClass] -> DerivClause
printDerivClause derives = DerivClause Nothing (map (ConT . genName) derives)
