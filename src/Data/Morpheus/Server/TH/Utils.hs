{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Utils
  ( kindName,
    constraintTypeable,
    typeNameStringE,
    withPure,
    mkTypeableConstraints,
    m',
    m_,
    funDProxy,
    isParametrizedHaskellType,
    isSubscription,
  )
where

import Data.Morpheus.Internal.TH
  ( _',
    apply,
    funDSimple,
    vars,
  )
import Data.Morpheus.Kind
  ( SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Types.Internal.AST
  ( OperationType (..),
    TypeKind (..),
    TypeName,
    unpackName,
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
  )
import Relude hiding (Type)

m_ :: Name
m_ = mkName "m"

m' :: Type
m' = VarT m_

isParametrizedHaskellType :: Info -> Bool
isParametrizedHaskellType (TyConI x) = not $ null $ getTypeVariables x
isParametrizedHaskellType _ = False

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
typeNameStringE = LitE . StringL . (unpack . unpackName)

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
