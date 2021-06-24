{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Internal.TH.Utils
  ( kindName,
    constraintTypeable,
    typeNameStringE,
    withPure,
    mkTypeableConstraints,
    m',
    m_,
    funDProxy,
    isParametrizedResolverType,
    isSubscription,
  )
where

import Data.Morpheus.Internal.TH
  ( _',
    apply,
    funDSimple,
    toName,
    vars,
  )
import Data.Morpheus.Kind
  ( SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    OperationType (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName (..),
    isResolverType,
    lookupWith,
  )
import Data.Text (unpack)
import Language.Haskell.TH
  ( CxtQ,
    Dec (..),
    DecQ,
    Exp (..),
    ExpQ,
    Info (..),
    Lit (..),
    Name,
    Q,
    TyVarBndr,
    Type (..),
    cxt,
    mkName,
    reify,
  )
import Relude hiding (Type)

m_ :: Name
m_ = mkName "m"

m' :: Type
m' = VarT m_

isParametrizedResolverType :: TypeName -> [TypeDefinition ANY s] -> Q Bool
isParametrizedResolverType "__TypeKind" _ = pure False
isParametrizedResolverType "Boolean" _ = pure False
isParametrizedResolverType "String" _ = pure False
isParametrizedResolverType "Int" _ = pure False
isParametrizedResolverType "Float" _ = pure False
isParametrizedResolverType key lib = case lookupWith typeName key lib of
  Just x -> pure (isResolverType x)
  Nothing -> isParametrizedType <$> reify (toName key)

isParametrizedType :: Info -> Bool
isParametrizedType (TyConI x) = not $ null $ getTypeVariables x
isParametrizedType _ = False

getTypeVariables :: Dec -> [TyVarBndr]
getTypeVariables (DataD _ _ args _ _ _) = args
getTypeVariables (NewtypeD _ _ args _ _ _) = args
getTypeVariables (TySynD _ args _) = args
getTypeVariables _ = []

funDProxy :: [(Name, ExpQ)] -> [DecQ]
funDProxy = map fun
  where
    fun (name, body) = funDSimple name [_'] body

withPure :: Exp -> Exp
withPure = AppE (VarE 'pure)

typeNameStringE :: TypeName -> Exp
typeNameStringE = LitE . StringL . (unpack . readTypeName)

constraintTypeable :: Type -> Q Type
constraintTypeable name = pure $ apply ''Typeable [name]

mkTypeableConstraints :: [Name] -> CxtQ
mkTypeableConstraints = cxt . map constraintTypeable . vars

kindName :: TypeKind -> Name
kindName KindScalar = ''SCALAR
kindName KindList = ''WRAPPER
kindName KindNonNull = ''WRAPPER
kindName _ = ''TYPE

isSubscription :: TypeKind -> Bool
isSubscription (KindObject (Just Subscription)) = True
isSubscription _ = False
