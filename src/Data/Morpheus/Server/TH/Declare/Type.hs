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
    camelCaseFieldName,
    camelCaseTypeName,
    declareTypeRef,
    toCon,
    toName,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerConsD,
    ServerDec,
    ServerDecContext (..),
    ServerFieldDefinition (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.Internal.TH.Utils
  ( isSubscription,
    m',
    m_,
  )
import Data.Morpheus.Types (TypeGuard)
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    FieldName,
    TypeKind (..),
    TypeName,
    isResolverType,
  )
import Language.Haskell.TH
import Relude hiding (Type)

declareType :: ServerTypeDefinition s -> ServerDec [Dec]
declareType (ServerInterfaceDefinition name interfaceName unionName) =
  pure
    [ TySynD
        (toName name)
        [PlainTV m_]
        (apply ''TypeGuard [apply interfaceName [m'], apply unionName [m']])
    ]
declareType ServerTypeDefinition {tKind = KindScalar} = pure []
declareType
  ServerTypeDefinition
    { tName,
      tCons,
      tKind,
      typeParameters
    } =
    do
      cons <- declareCons tKind tName tCons
      let vars = map (PlainTV . toName) typeParameters
      let name = toName tName
      pure [DataD [] name vars Nothing cons (derive tKind)]

derive :: TypeKind -> [DerivClause]
derive tKind = [deriveClasses (''Generic : derivingList)]
  where
    derivingList
      | isResolverType tKind = []
      | otherwise = [''Show]

deriveClasses :: [Name] -> DerivClause
deriveClasses classNames = DerivClause Nothing (map ConT classNames)

declareCons ::
  TypeKind ->
  TypeName ->
  [ServerConsD] ->
  ServerDec [Con]
declareCons tKind tName = traverse consR
  where
    consR ConsD {cName, cFields} =
      RecC
        <$> consName tKind tName cName
        <*> traverse (declareField tKind tName) cFields

declareField ::
  TypeKind ->
  TypeName ->
  ServerFieldDefinition ->
  ServerDec (Name, Bang, Type)
declareField tKind tName field = do
  fieldName <- fieldTypeName tName (fieldName field)
  pure
    ( fieldName,
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

fieldTypeName :: TypeName -> FieldName -> ServerDec Name
fieldTypeName tName fieldName = do
  namespace <- asks namespace
  pure $ toName $
    if namespace
      then camelCaseFieldName tName fieldName
      else fieldName

consName :: TypeKind -> TypeName -> TypeName -> ServerDec Name
consName kind name conName =
  toName . genName <$> asks namespace
  where
    genName True | kind == KindEnum = camelCaseTypeName [name] conName
    genName _ = conName

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
