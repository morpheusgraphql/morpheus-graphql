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
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Internal.TH
  ( _',
    apply,
    applyCons,
    applyVars,
    declareTypeRef,
    funDSimple,
    camelCaseFieldName,
    camelCaseTypeName,
    toCon,
    toVar,
    ToName (..),
    toString,
    typeInstanceDec,
    v',
    vars,
  )
where

import Data.Char
  ( toLower,
    toUpper,
  )
import qualified Data.Morpheus.Types.Internal.AST as N
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    packName,
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

declareTypeRef :: (TypeName -> Type) -> TypeRef -> Type
declareTypeRef f TypeRef {typeConName, typeWrappers} = wrappedT typeWrappers
  where
    wrappedT :: TypeWrapper -> Type
    wrappedT (TypeList xs nonNull) = withNonNull nonNull (AppT (ConT ''[]) $ wrappedT xs)
    wrappedT (BaseType nonNull) = withNonNull nonNull (f typeConName)
    {-# INLINE wrappedT #-}
{-# INLINE declareTypeRef #-}

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

instance ToName TypeName where
  toName = mkName . toHaskellTypeName

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

#if MIN_VERSION_template_haskell(2,15,0)
-- fix breaking changes
typeInstanceDec :: Name -> Type -> Type -> Dec
typeInstanceDec typeFamily arg res = TySynInstD (TySynEqn Nothing (AppT (ConT typeFamily) arg) res)
#else
--
typeInstanceDec :: Name -> Type -> Type -> Dec
typeInstanceDec typeFamily arg res = TySynInstD typeFamily (TySynEqn [arg] res)
#endif

---Name

mapFstChar :: (Char -> Char) -> Text -> Text
mapFstChar f x
  | T.null x = x
  | otherwise = T.singleton (f $ T.head x) <> T.tail x

capitalize :: Text -> Text
capitalize = mapFstChar toUpper

camelCaseTypeName :: [N.Name t] -> TypeName -> TypeName
camelCaseTypeName list name =
  packName $ T.concat $
    map (capitalize . unpackName) (list <> [coerce name])

toHaskellTypeName :: TypeName -> String
toHaskellTypeName "String" = "Text"
toHaskellTypeName "Boolean" = "Bool"
toHaskellTypeName "Float" = "Double"
toHaskellTypeName name = T.unpack $ capitalize $ unpackName name
{-# INLINE toHaskellTypeName #-}

uncapitalize :: Text -> Text
uncapitalize = mapFstChar toLower

camelCaseFieldName :: TypeName -> FieldName -> FieldName
camelCaseFieldName nSpace name =
  packName $
    uncapitalize (unpackName nSpace)
      <> capitalize (unpackName name)

toHaskellName :: FieldName -> String
toHaskellName name
  | isReserved name = T.unpack (unpackName name <> "'")
  | otherwise = T.unpack (uncapitalize (unpackName name))
{-# INLINE toHaskellName #-}

-- handle reserved Names
isReserved :: FieldName -> Bool
isReserved "case" = True
isReserved "class" = True
isReserved "data" = True
isReserved "default" = True
isReserved "deriving" = True
isReserved "do" = True
isReserved "else" = True
isReserved "foreign" = True
isReserved "if" = True
isReserved "import" = True
isReserved "in" = True
isReserved "infix" = True
isReserved "infixl" = True
isReserved "infixr" = True
isReserved "instance" = True
isReserved "let" = True
isReserved "module" = True
isReserved "newtype" = True
isReserved "of" = True
isReserved "then" = True
isReserved "type" = True
isReserved "where" = True
isReserved "_" = True
isReserved _ = False
{-# INLINE isReserved #-}
