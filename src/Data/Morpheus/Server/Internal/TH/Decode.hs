{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Server.Internal.TH.Decode
  ( withInputObject,
    withMaybe,
    withList,
    withEnum,
    withInputUnion,
    decodeFieldWith,
    withScalar,
  )
where

-- MORPHEUS
import Data.Morpheus.Internal.Utils
  ( empty,
    selectBy,
    selectOr,
  )
import Data.Morpheus.Types.GQLScalar
  ( toScalar,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    InternalError,
    Message,
    ObjectEntry (..),
    ScalarValue,
    Token,
    TypeName (..),
    VALID,
    ValidObject,
    ValidValue,
    Value (..),
    msg,
    msgInternal,
    toFieldName,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Failure (..),
  )

withInputObject ::
  Failure InternalError m =>
  (ValidObject -> m a) ->
  ValidValue ->
  m a
withInputObject f (Object object) = f object
withInputObject _ isType = failure (typeMismatch "Object" isType)

withMaybe :: Monad m => (ValidValue -> m a) -> ValidValue -> m (Maybe a)
withMaybe _ Null = pure Nothing
withMaybe decode x = Just <$> decode x

withList ::
  (Failure InternalError m, Monad m) =>
  (ValidValue -> m a) ->
  ValidValue ->
  m [a]
withList decode (List li) = traverse decode li
withList _ isType = failure (typeMismatch "List" isType)

withEnum :: Failure InternalError m => (TypeName -> m a) -> Value VALID -> m a
withEnum decode (Enum value) = decode value
withEnum _ isType = failure (typeMismatch "Enum" isType)

withInputUnion ::
  (Failure InternalError m, Monad m) =>
  (TypeName -> ValidObject -> ValidObject -> m a) ->
  ValidObject ->
  m a
withInputUnion decoder unions =
  selectBy
    ("__typename not found on Input Union" :: InternalError)
    "__typename"
    unions
    >>= providesValueFor . entryValue
  where
    providesValueFor (Enum key) = selectOr notfound onFound (toFieldName key) unions
      where
        notfound = withInputObject (decoder key unions) (Object empty)
        onFound = withInputObject (decoder key unions) . entryValue
    providesValueFor _ = failure ("__typename must be Enum" :: InternalError)

withScalar ::
  (Applicative m, Failure InternalError m) =>
  TypeName ->
  (ScalarValue -> Either Token a) ->
  Value VALID ->
  m a
withScalar typename parseValue value = case toScalar value >>= parseValue of
  Right scalar -> pure scalar
  Left message ->
    failure
      ( typeMismatch
          ("SCALAR(" <> msg typename <> ")" <> msg message)
          value
      )

decodeFieldWith :: (Value VALID -> m a) -> FieldName -> ValidObject -> m a
decodeFieldWith decoder = selectOr (decoder Null) (decoder . entryValue)

-- if value is already validated but value has different type
typeMismatch :: Message -> Value s -> InternalError
typeMismatch text jsType =
  "Type mismatch! expected:" <> msgInternal text <> ", got: "
    <> msgInternal jsType
