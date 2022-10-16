{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.TH
  ( _',
    apply,
    applyVars,
    toCon,
    ToVar (..),
    ToName (..),
    ToString (..),
    v',
    PrintExp (..),
    PrintType (..),
    PrintDec (..),
    m',
    m_,
    printTypeSynonym,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( AssociatedType (..),
    CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenType (..),
    CodeGenTypeName (..),
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    MethodArgument (..),
    TypeClassInstance (..),
    TypeValue (..),
    getFullName,
  )
import Data.Morpheus.CodeGen.Internal.Name (camelCaseFieldName)
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
import qualified Data.Morpheus.Types.Internal.AST as AST
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

{- ORMOLU_DISABLE -}
instance (ToName a) => ToCon a Pat where
#if MIN_VERSION_template_haskell(2,18,0)
  toCon name = ConP (toName name) [] []
#else
  toCon name = ConP (toName name) []
#endif
{- ORMOLU_ENABLE -}

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
applyVars name li = apply name (map toVar li)

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
#if MIN_VERSION_template_haskell(2,17,0)
toTypeVars :: [Name] -> [TyVarBndr ()]
toTypeVars = map (flip PlainTV ())
#else
toTypeVars :: [Name] -> [TyVarBndr]
toTypeVars = map PlainTV
#endif
{- ORMOLU_ENABLE -}
class PrintExp a where
  printExp :: a -> ExpQ

class PrintType a where
  printType :: a -> TypeQ

class PrintDec a where
  printDec :: a -> Q Dec

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

printField :: CodeGenField -> (Name, Bang, Type)
printField CodeGenField {..} =
  ( toName fieldName,
    Bang NoSourceUnpackedness NoSourceStrictness,
    foldr applyWrapper (toCon fieldType) wrappers
  )

applyWrapper :: FIELD_TYPE_WRAPPER -> Type -> Type
applyWrapper PARAMETRIZED = (`AppT` m')
applyWrapper MONAD = AppT m'
applyWrapper (SUBSCRIPTION name) = AppT (ConT name)
applyWrapper (ARG typeName) = InfixT (ConT (toName typeName)) ''Function
applyWrapper (GQL_WRAPPER wrappers) = wrappedType wrappers
applyWrapper (TAGGED_ARG argName fieldName typeRef) = InfixT arg ''Function
  where
    arg =
      AppT
        ( AppT
            (ConT argName)
            (LitT $ StrTyLit $ T.unpack $ unpackName fieldName)
        )
        (declareTypeRef toCon typeRef)

type Function = (->)

m_ :: Name
m_ = mkName "m"

m' :: Type
m' = VarT m_

constraint :: (Name, Name) -> Q Type
constraint (con, name) = pure $ apply con [toVar name]

printConstraints :: [(Name, Name)] -> Q Cxt
printConstraints = cxt . map constraint

printConstructor :: CodeGenConstructor -> Con
printConstructor CodeGenConstructor {..}
  | null constructorFields = NormalC (toName constructorName) []
  | otherwise = RecC (toName constructorName) (map printField constructorFields)

printTypeSynonym :: ToName a => a -> [Name] -> Type -> Dec
printTypeSynonym name params = TySynD (toName name) (toTypeVars params)

instance ToName CodeGenTypeName where
  toName = toName . getFullName

instance PrintType CodeGenTypeName where
  printType name = applyVars (toName name) (map toName $ typeParameters name)

instance ToName AST.DirectiveLocation where
  toName AST.QUERY = 'AST.QUERY
  toName AST.MUTATION = 'AST.MUTATION
  toName AST.SUBSCRIPTION = 'AST.SUBSCRIPTION
  toName AST.FIELD = 'AST.FIELD
  toName AST.FRAGMENT_DEFINITION = 'AST.FRAGMENT_DEFINITION
  toName AST.FRAGMENT_SPREAD = 'AST.FRAGMENT_SPREAD
  toName AST.INLINE_FRAGMENT = 'AST.INLINE_FRAGMENT
  toName AST.SCHEMA = 'AST.SCHEMA
  toName AST.SCALAR = 'AST.SCALAR
  toName AST.OBJECT = 'AST.OBJECT
  toName AST.FIELD_DEFINITION = 'AST.FIELD_DEFINITION
  toName AST.ARGUMENT_DEFINITION = 'AST.ARGUMENT_DEFINITION
  toName AST.INTERFACE = 'AST.INTERFACE
  toName AST.UNION = 'AST.UNION
  toName AST.ENUM = 'AST.ENUM
  toName AST.ENUM_VALUE = 'AST.ENUM_VALUE
  toName AST.INPUT_OBJECT = 'AST.INPUT_OBJECT
  toName AST.INPUT_FIELD_DEFINITION = 'AST.INPUT_FIELD_DEFINITION

instance PrintType AssociatedType where
  printType (AssociatedLocations xs) = pure $ foldr (AppT . AppT PromotedConsT . PromotedT . toName) PromotedNilT xs
  printType (AssociatedTypeName name) = toCon name

instance PrintDec (TypeClassInstance ExpQ) where
  printDec TypeClassInstance {..} =
    instanceD
      (printConstraints typeClassContext)
      headType
      (map assocTypes assoc <> map printFun typeClassMethods)
    where
      printFun :: (Name, MethodArgument, ExpQ) -> DecQ
      printFun (funName, args, body) = funD funName [clause (printArg args) (normalB body) []]
      assocTypes :: (Name, AssociatedType) -> DecQ
      assocTypes (assocName, type') = do
        ty <- printType typeClassTarget
        assocType <- printType type'
        pure $ typeInstanceDec assocName ty assocType
      headType :: TypeQ
      headType = do
        ty <- printType typeClassTarget
        pure $ apply typeClassName [ty]

printArg :: MethodArgument -> [PatQ]
printArg (DestructArgument cons) = [destructConstructor cons]
printArg NoArgument = []
printArg ProxyArgument = [_']

instance PrintDec CodeGenType where
  printDec CodeGenType {..} =
    pure $
      DataD
        []
        (toName cgTypeName)
        (toTypeVars $ map toName $ typeParameters cgTypeName)
        Nothing
        (map printConstructor cgConstructors)
        [printDerivClause cgDerivations]

-- |
-- input:
-- >>>
-- WAS WAS destructRecord "User" ["name","id"]
-- >>>
--
-- expression:
-- >>>
-- WAS WAS (User name id)
-- >>>
destructConstructor :: CodeGenConstructor -> PatQ
destructConstructor (CodeGenConstructor conName fields) = conP (toName conName) names
  where
    names = map (typeField conName . fieldName) fields

typeField :: ToVar FieldName c => CodeGenTypeName -> FieldName -> c
typeField conName = toVar . camelCaseFieldName (getFullName conName)
