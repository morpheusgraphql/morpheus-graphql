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
    PrintableValue (..),
    TypeClassInstance (..),
    TypeValue (..),
    getFullName,
  )
import Data.Morpheus.CodeGen.Utils
  ( toHaskellName,
    toHaskellTypeName,
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation (..),
    FieldName,
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

v' :: (ToVar Name a) => a
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

instance (ToString a b) => ToString a (Q b) where
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

instance (ToCon a b) => ToCon a (Q b) where
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

instance (ToVar a b) => ToVar a (Q b) where
  toVar = pure . toVar

instance (ToName a) => ToVar a Type where
  toVar = VarT . toName

instance (ToName a) => ToVar a Exp where
  toVar = VarE . toName

instance (ToName a) => ToVar a Pat where
  toVar = VarP . toName

class Apply a where
  apply :: (ToCon i a) => i -> [a] -> a

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
#if MIN_VERSION_template_haskell(2,21,0)
toTypeVars :: [Name] -> [TyVarBndr BndrVis]
toTypeVars = map (flip PlainTV BndrReq)
#elif MIN_VERSION_template_haskell(2,17,0)
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
  printExp (PrintableTypeValue x) = printExp x

genName :: DerivingClass -> Name
genName GENERIC = ''Generic
genName SHOW = ''Show
genName CLASS_EQ = ''Eq

printDerivClause :: [DerivingClass] -> DerivClause
printDerivClause derives = DerivClause Nothing (map (ConT . genName) derives)

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
printConstructor CodeGenConstructor {constructorFields = [field], ..}
  | fieldName field == "_" = NormalC (toName constructorName) [ignoreName $ printField field]
  where
    ignoreName (_, b, t) = (b, t)
printConstructor CodeGenConstructor {..}
  | null constructorFields = NormalC (toName constructorName) []
  | otherwise = RecC (toName constructorName) (map printField constructorFields)

printField :: CodeGenField -> (Name, Bang, Type)
printField CodeGenField {..} =
  ( toName fieldName,
    Bang NoSourceUnpackedness NoSourceStrictness,
    foldr applyWrapper (toCon fieldType) wrappers
  )

printTypeSynonym :: (ToName a) => a -> [Name] -> Type -> Dec
printTypeSynonym name params = TySynD (toName name) (toTypeVars params)

instance ToName CodeGenTypeName where
  toName = toName . getFullName

instance PrintType CodeGenTypeName where
  printType name = applyVars (toName name) (map toName $ typeParameters name)

instance ToName DirectiveLocation where
  toName LOCATION_QUERY = 'LOCATION_QUERY
  toName LOCATION_MUTATION = 'LOCATION_MUTATION
  toName LOCATION_SUBSCRIPTION = 'LOCATION_SUBSCRIPTION
  toName LOCATION_FIELD = 'LOCATION_FIELD
  toName LOCATION_FRAGMENT_DEFINITION = 'LOCATION_FRAGMENT_DEFINITION
  toName LOCATION_FRAGMENT_SPREAD = 'LOCATION_FRAGMENT_SPREAD
  toName LOCATION_INLINE_FRAGMENT = 'LOCATION_INLINE_FRAGMENT
  toName LOCATION_SCHEMA = 'LOCATION_SCHEMA
  toName LOCATION_SCALAR = 'LOCATION_SCALAR
  toName LOCATION_OBJECT = 'LOCATION_OBJECT
  toName LOCATION_FIELD_DEFINITION = 'LOCATION_FIELD_DEFINITION
  toName LOCATION_ARGUMENT_DEFINITION = 'LOCATION_ARGUMENT_DEFINITION
  toName LOCATION_INTERFACE = 'LOCATION_INTERFACE
  toName LOCATION_UNION = 'LOCATION_UNION
  toName LOCATION_ENUM = 'LOCATION_ENUM
  toName LOCATION_ENUM_VALUE = 'LOCATION_ENUM_VALUE
  toName LOCATION_INPUT_OBJECT = 'LOCATION_INPUT_OBJECT
  toName LOCATION_INPUT_FIELD_DEFINITION = 'LOCATION_INPUT_FIELD_DEFINITION

instance PrintType AssociatedType where
  printType (AssociatedLocations xs) = pure $ foldr (AppT . AppT PromotedConsT . PromotedT . toName) PromotedNilT xs
  printType (AssociatedTypeName name) = toCon name

instance (PrintExp body) => PrintDec (TypeClassInstance body) where
  printDec TypeClassInstance {..} =
    instanceD
      (printConstraints typeClassContext)
      headType
      (map assocTypes assoc <> map printFun typeClassMethods)
    where
      printFun (funName, args, body) = funD funName [clause (printArg args) (normalB (printExp body)) []]
      assocTypes (assocName, type') = do
        ty <- printType typeClassTarget
        assocType <- printType type'
        pure $ typeInstanceDec assocName ty assocType
      headType = do
        ty <- printType typeClassTarget
        pure $ apply typeClassName [ty]

printArg :: MethodArgument -> [PatQ]
printArg (DestructArgument con fields) = [conP con (map toVar fields)]
printArg NoArgument = []
printArg ProxyArgument = [_']

instance PrintDec CodeGenType where
  printDec CodeGenType {..} =
    pure
      $ DataD
        []
        (toName cgTypeName)
        (toTypeVars $ map toName $ typeParameters cgTypeName)
        Nothing
        (map printConstructor cgConstructors)
        [printDerivClause cgDerivations]

instance PrintExp PrintableValue where
  printExp (PrintableValue x) = [|x|]
