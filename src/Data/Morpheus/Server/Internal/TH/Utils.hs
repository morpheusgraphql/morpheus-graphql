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
    tyConArgs,
    funDProxy,
  )
where

import Data.Morpheus.Internal.TH
  ( _',
    apply,
    funDSimple,
    vars,
  )
import Data.Morpheus.Kind
  ( INTERFACE,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
    TypeName (..),
    isOutputObject,
  )
import Data.Text (unpack)
import Language.Haskell.TH
  ( CxtQ,
    DecQ,
    Exp (..),
    ExpQ,
    Lit (..),
    Name,
    Type (..),
    cxt,
    mkName,
  )
import Relude hiding (Type)

funDProxy :: [(Name, ExpQ)] -> [DecQ]
funDProxy = map fun
  where
    fun (name, body) = funDSimple name [_'] body

tyConArgs :: TypeKind -> [String]
tyConArgs kindD
  | isOutputObject kindD || kindD == KindUnion = ["m"]
  | otherwise = []

m' :: Type
m' = VarT (mkName "m")

withPure :: Exp -> Exp
withPure = AppE (VarE 'pure)

typeNameStringE :: TypeName -> Exp
typeNameStringE = LitE . StringL . (unpack . readTypeName)

constraintTypeable :: Type -> Type
constraintTypeable name = apply ''Typeable [name]

mkTypeableConstraints :: [String] -> CxtQ
mkTypeableConstraints args = cxt $ map (pure . constraintTypeable) (vars args)

kindName :: TypeKind -> Name
kindName KindScalar = ''SCALAR
kindName KindList = ''WRAPPER
kindName KindNonNull = ''WRAPPER
kindName KindInterface = ''INTERFACE
kindName _ = ''TYPE
