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
    tyConArgs,
    funDProxy,
    isParametrizedResolverType,
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
  ( INTERFACE,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    TypeDefinition (..),
    TypeKind (..),
    TypeName (..),
    isOutputObject,
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

m_ :: String
m_ = "m"

m' :: Type
m' = VarT (mkName m_)

isParametrizedResolverType :: TypeName -> [TypeDefinition ANY s] -> Q Bool
isParametrizedResolverType "__TypeKind" _ = pure False
isParametrizedResolverType "Boolean" _ = pure False
isParametrizedResolverType "String" _ = pure False
isParametrizedResolverType "Int" _ = pure False
isParametrizedResolverType "Float" _ = pure False
isParametrizedResolverType key lib = case typeContent <$> lookupWith typeName key lib of
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

tyConArgs :: TypeKind -> [String]
tyConArgs kindD
  | isOutputObject kindD || kindD == KindUnion = ["m"]
  | otherwise = []

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
