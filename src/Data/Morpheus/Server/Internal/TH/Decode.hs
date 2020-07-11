{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Server.Internal.TH.Decode
  ( withObject,
    withMaybe,
    withList,
    withEnum,
    withInputUnion,
    decodeFieldWith,
  )
where

-- MORPHEUS
import Data.Morpheus.Error
  ( typeMismatch,
  )
import Data.Morpheus.Internal.Utils
  ( empty,
    selectBy,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    InternalError,
    ObjectEntry (..),
    TypeName (..),
    VALID,
    ValidObject,
    ValidValue,
    Value (..),
    toFieldName,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Failure (..),
  )

withObject ::
  Failure InternalError m =>
  (ValidObject -> m a) ->
  ValidValue ->
  m a
withObject f (Object object) = f object
withObject _ isType = failure (typeMismatch "Object" isType)

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
  entryValue
    <$> selectBy
      ("__typename not found on Input Union" :: InternalError)
      "__typename"
      unions
    >>= providesValueFor
  where
    providesValueFor (Enum key) = selectOr notfound onFound (toFieldName key) unions
      where
        notfound = withObject (decoder key unions) (Object empty)
        onFound = withObject (decoder key unions) . entryValue
    providesValueFor _ = failure ("__typename must be Enum" :: InternalError)

decodeFieldWith :: (Value VALID -> m a) -> FieldName -> ValidObject -> m a
decodeFieldWith decoder = selectOr (decoder Null) (decoder . entryValue)
