{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare.Type
  ( declareType,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( SubscriptionField,
  )
import Data.Morpheus.Internal.TH
  ( apply,
    declareTypeRef,
    toCon,
    toName,
  )
import Data.Morpheus.Server.CodeGen.Types
  ( ServerConstructorDefinition (..),
    ServerFieldDefinition (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.TH.Utils
  ( isSubscription,
    m',
    m_,
  )
import Data.Morpheus.Types (TypeGuard)
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
    TypeName,
    isResolverType,
  )
import Language.Haskell.TH
import Relude hiding (Type)

declareType :: ServerTypeDefinition s -> [Dec]
declareType (ServerInterfaceDefinition name interfaceName unionName) =
  [ TySynD
      (toName name)
      [PlainTV m_]
      (apply ''TypeGuard [apply interfaceName [m'], apply unionName [m']])
  ]
declareType ServerTypeDefinition {tKind = KindScalar} = []
declareType
  ServerTypeDefinition
    { tName,
      tCons,
      tKind,
      derives,
      typeParameters
    } = [DataD [] (toName tName) vars Nothing cons [deriveClasses derives]]
    where
      cons = map (declareCons tKind) tCons
      vars = map (PlainTV . toName) typeParameters

deriveClasses :: [Name] -> DerivClause
deriveClasses classNames = DerivClause Nothing (map ConT classNames)

declareCons :: TypeKind -> ServerConstructorDefinition -> Con
declareCons tKind ServerConstructorDefinition {constructorName, constructorFields} =
  RecC
    (toName constructorName)
    ( map (declareField tKind) constructorFields
    )

declareField :: TypeKind -> ServerFieldDefinition -> (Name, Bang, Type)
declareField tKind field =
  ( toName (fieldName field),
    Bang NoSourceUnpackedness NoSourceStrictness,
    renderFieldType tKind field
  )

renderFieldType ::
  TypeKind ->
  ServerFieldDefinition ->
  Type
renderFieldType
  tKind
  ServerFieldDefinition
    { isParametrized,
      fieldType,
      argumentsTypeName
    } =
    withFieldWrappers tKind argumentsTypeName (declareTypeRef renderTypeName fieldType)
    where
      renderTypeName :: TypeName -> Type
      renderTypeName
        | isParametrized = (`apply` [m'])
        | otherwise = toCon

-- withArgs: t => a -> t
withArgs :: TypeName -> Type -> Type
withArgs argsTypename = InfixT (ConT (toName argsTypename)) ''Function

type Function = (->)

withMonad :: Type -> Type
withMonad = AppT m'

------------------------------------------------
withFieldWrappers ::
  TypeKind ->
  Maybe TypeName ->
  Type ->
  Type
withFieldWrappers kind (Just argsTypename) =
  withArgs argsTypename
    . withSubscriptionField kind
    . withMonad
withFieldWrappers kind _
  | isResolverType kind && (KindUnion /= kind) =
    withSubscriptionField kind
      . withMonad
  | otherwise = id

-- withSubscriptionField: t => SubscriptionField t
withSubscriptionField :: TypeKind -> Type -> Type
withSubscriptionField kind x
  | isSubscription kind = AppT (ConT ''SubscriptionField) x
  | otherwise = x
