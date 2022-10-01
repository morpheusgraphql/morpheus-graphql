{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.Decode
  ( withInputObject,
    withEnum,
    withInputUnion,
    decodeFieldWith,
    withScalar,
    handleEither,
    getFieldName,
    DecoderT,
    withKind,
    Info (..),
    Context (..),
    Tag (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving (ResolverState)
import Data.Morpheus.Internal.Utils
  ( selectOr,
  )
import Data.Morpheus.Server.Types.Internal
import Data.Morpheus.Types.GQLScalar
  ( toScalar,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    Msg (msg),
    ObjectEntry (..),
    ScalarValue,
    Token,
    TypeName,
    VALID,
    ValidObject,
    ValidValue,
    Value (..),
    getInputUnionValue,
    internal,
  )
import Relude

withInputObject ::
  MonadError GQLError m =>
  (ValidObject -> m a) ->
  ValidValue ->
  m a
withInputObject f (Object object) = f object
withInputObject _ isType = throwError (typeMismatch "InputObject" isType)

-- | Useful for more restrictive instances of lists (non empty, size indexed etc)
withEnum :: MonadError GQLError m => (TypeName -> m a) -> Value VALID -> m a
withEnum decode (Enum value) = decode value
withEnum _ isType = throwError (typeMismatch "Enum" isType)

withInputUnion ::
  (MonadError GQLError m, Monad m) =>
  (TypeName -> ValidObject -> ValidObject -> m a) ->
  ValidObject ->
  m a
withInputUnion decoder unions =
  either onFail onSuccess (getInputUnionValue unions)
  where
    onSuccess (name, value) = withInputObject (decoder name unions) value
    onFail = throwError . internal . msg

withScalar ::
  (Applicative m, MonadError GQLError m) =>
  TypeName ->
  (ScalarValue -> Either Token a) ->
  Value VALID ->
  m a
withScalar typename decodeScalar value = case toScalar value >>= decodeScalar of
  Right scalar -> pure scalar
  Left message ->
    throwError
      ( typeMismatch
          ("SCALAR(" <> msg typename <> ")" <> msg message)
          value
      )

decodeFieldWith :: (Value VALID -> m a) -> FieldName -> ValidObject -> m a
decodeFieldWith decoder = selectOr (decoder Null) (decoder . entryValue)

handleEither :: MonadError GQLError m => Either GQLError a -> m a
handleEither = either throwError pure

-- if value is already validated but value has different type
typeMismatch :: GQLError -> Value s -> GQLError
typeMismatch text jsType =
  internal $
    "Type mismatch! expected:"
      <> text
      <> ", got: "
      <> msg jsType

getFieldName :: FieldName -> Int -> FieldName
getFieldName "" index = "_" <> show index
getFieldName label _ = label

data Tag = D_CONS | D_UNION deriving (Eq, Ord)

data Info = Info
  { kind :: Tag,
    tagName :: [TypeName]
  }

instance Semigroup Info where
  Info D_UNION t1 <> Info _ t2 = Info D_UNION (t1 <> t2)
  Info _ t1 <> Info D_UNION t2 = Info D_UNION (t1 <> t2)
  Info D_CONS t1 <> Info D_CONS t2 = Info D_CONS (t1 <> t2)

data Context = Context
  { contKind :: Tag,
    typeName :: TypeName,
    options :: GQLTypeOptions
  }

type DecoderT = ReaderT Context ResolverState

withKind :: Tag -> DecoderT a -> DecoderT a
withKind contKind = local (\ctx -> ctx {contKind})
