{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Internal.TH.Decode
  ( withInputObject,
    withMaybe,
    withList,
    withRefinedList,
    withEnum,
    withInputUnion,
    decodeFieldWith,
    withScalar,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative (..))
import Control.Monad (Monad ((>>=)))
import Data.Either (Either (..))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
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
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Prelude ((.))

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

-- | Useful for more restrictive instances of lists (non empty, size indexed etc)
withRefinedList :: ([a] -> Either Message (rList a)) -> (ValidValue -> Eventless a) -> ValidValue -> Eventless (rList a)
withRefinedList refiner decode (List li) = do
  listRes <- traverse decode li
  case refiner listRes of
    Left err -> failure err
    Right value -> pure value
withRefinedList _ _ isType = internalTypeMismatch "List" isType

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
    ("__typename" :: FieldName)
    unions
    >>= providesValueFor . entryValue
  where
    providesValueFor (Enum key) = selectOr notFound onFound (toFieldName key) unions
      where
        notFound = withInputObject (decoder key unions) (Object empty)
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
