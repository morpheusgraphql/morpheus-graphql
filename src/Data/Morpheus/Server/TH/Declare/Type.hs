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
    nameSpaceField,
    nameSpaceType,
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
    tyConArgs,
  )
import Data.Morpheus.Types (Guard (..))
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    FieldDefinition (..),
    FieldName (..),
    TypeKind (..),
    TypeName (..),
    isResolverType,
  )
import Language.Haskell.TH hiding (Guard)
import Relude hiding (Type)

declareType :: ServerTypeDefinition cat s -> ServerDec [Dec]
declareType (ServerInterfaceDefinition name interfaceName unionName) =
  pure
    [ TySynD
        (toName name)
        [PlainTV m_]
        (apply ''Guard [apply interfaceName [m'], apply unionName [m']])
    ]
declareType ServerTypeDefinition {tKind = KindScalar} = pure []
declareType
  ServerTypeDefinition
    { tName,
      tCons,
      tKind
    } =
    do
      cons <- declareCons tKind tName tCons
      let vars = map (PlainTV . toName) (tyConArgs tKind)
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
  [ServerConsD cat s] ->
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
  ServerFieldDefinition cat s ->
  ServerDec (Name, Bang, Type)
declareField tKind tName field = do
  fieldName <- fieldTypeName tName (fieldName $ originalField field)
  pure
    ( fieldName,
      Bang NoSourceUnpackedness NoSourceStrictness,
      renderFieldType tKind field
    )

renderFieldType ::
  TypeKind ->
  ServerFieldDefinition cat s ->
  Type
renderFieldType
  tKind
  ServerFieldDefinition
    { isParametrized,
      originalField = FieldDefinition {fieldType},
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
      then nameSpaceField tName fieldName
      else fieldName

consName :: TypeKind -> TypeName -> TypeName -> ServerDec Name
consName kind (TypeName name) conName =
  toName . genName <$> asks namespace
  where
    genName True | kind == KindEnum = nameSpaceType [FieldName name] conName
    genName _ = conName

-- withArgs: t => a -> t
withArgs :: TypeName -> Type -> Type
withArgs argsTypename = AppT (AppT (ConT ''(->)) (ConT (toName argsTypename)))

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
