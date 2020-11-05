{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare.Type
  ( declareType,
  )
where

import Data.Morpheus.Internal.TH
  ( declareTypeRef,
    m',
    nameSpaceField,
    nameSpaceType,
    toName,
    tyConArgs,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerDec,
    ServerDecContext (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    ConsD (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    TRUE,
    TypeKind (..),
    TypeName (..),
    isOutput,
    isOutputObject,
    isSubscription,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( SubscriptionField,
  )
import Language.Haskell.TH
import Relude hiding (Type)

declareType :: ServerTypeDefinition cat s -> ServerDec [Dec]
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
      pure
        [ DataD
            []
            (toName tName)
            vars
            Nothing
            cons
            (derive tKind)
        ]

derive :: TypeKind -> [DerivClause]
derive tKind = [deriveClasses (''Generic : derivingList)]
  where
    derivingList
      | isOutput tKind = []
      | otherwise = [''Show]

deriveClasses :: [Name] -> DerivClause
deriveClasses classNames = DerivClause Nothing (map ConT classNames)

declareCons ::
  TypeKind ->
  TypeName ->
  [ConsD cat s] ->
  ServerDec [Con]
declareCons tKind tName = traverse consR
  where
    consR ConsD {cName, cFields} =
      RecC
        <$> consName tKind tName cName
        <*> traverse (declareField tKind tName) cFields

consName :: TypeKind -> TypeName -> TypeName -> ServerDec Name
consName KindEnum (TypeName name) conName = do
  namespace' <- asks namespace
  if namespace'
    then pure $ toName $ nameSpaceType [FieldName name] conName
    else pure (toName conName)
consName _ _ conName = pure (toName conName)

declareField ::
  TypeKind ->
  TypeName ->
  FieldDefinition cat s ->
  ServerDec (Name, Bang, Type)
declareField tKind tName field@FieldDefinition {fieldName} = do
  namespace' <- asks namespace
  pure
    ( fieldTypeName namespace' tName fieldName,
      Bang NoSourceUnpackedness NoSourceStrictness,
      renderFieldType tKind field
    )

renderFieldType ::
  TypeKind ->
  FieldDefinition cat s ->
  Type
renderFieldType tKind FieldDefinition {fieldContent, fieldType} =
  withFieldWrappers tKind fieldContent (declareTypeRef fieldType)

fieldTypeName :: Bool -> TypeName -> FieldName -> Name
fieldTypeName namespace tName fieldName
  | namespace = toName (nameSpaceField tName fieldName)
  | otherwise = toName fieldName

-- withSubscriptionField: t => SubscriptionField t
withSubscriptionField :: TypeKind -> Type -> Type
withSubscriptionField kind x
  | isSubscription kind = AppT (ConT ''SubscriptionField) x
  | otherwise = x

-- withArgs: t => a -> t
withArgs :: TypeName -> Type -> Type
withArgs argsTypename = AppT (AppT arrowType argType)
  where
    argType = ConT $ toName argsTypename
    arrowType = ConT ''Arrow

-- withMonad: t => m t
withMonad :: Type -> Type
withMonad = AppT m'

type Arrow = (->)

------------------------------------------------
withFieldWrappers ::
  TypeKind ->
  Maybe (FieldContent TRUE cat s) ->
  Type ->
  Type
withFieldWrappers kind (Just (FieldArgs ArgumentsDefinition {argumentsTypename = Just argsTypename})) =
  withArgs argsTypename
    . withSubscriptionField kind
    . withMonad
withFieldWrappers kind _
  | isOutputObject kind =
    withSubscriptionField kind
      . withMonad
  | otherwise = id
