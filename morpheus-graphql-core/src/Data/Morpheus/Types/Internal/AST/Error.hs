{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Error
  ( at,
    atPositions,
    ValidationError,
    ValidationErrors,
    Error,
    GQLError,
    GQLErrors,
    msgValidation,
    msgInternal,
    InternalError,
    manyMsg,
    internal,
    isInternal,
    Errors,
    Msg (..),
    message,
    locations,
  )
where

import Data.Aeson
  ( FromJSON,
    Options (..),
    ToJSON (..),
    Value,
    defaultOptions,
    encode,
    genericToJSON,
  )
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Types.Internal.AST.Base
  ( Message (..),
    Position (..),
  )
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Relude hiding (ByteString, decodeUtf8)

type Error = GQLError

mkError :: Message -> GQLError
mkError x =
  GQLError
    { message = x,
      locations = [],
      errorType = Nothing,
      extensions = Nothing
    }

instance IsString Error where
  fromString = mkError . Message . T.pack

instance Semigroup Error where
  GQLError m1 l1 t1 e1 <> GQLError m2 l2 t2 e2 = GQLError (m1 <> m2) (l1 <> l2) (t1 <> t2) (e1 <> e2)

internal :: Error -> Error
internal x = x {errorType = Just Internal}

isInternal :: Error -> Bool
isInternal GQLError {errorType = Just Internal} = True
isInternal _ = False

at :: Error -> Position -> Error
at err pos = atPositions err [pos]
{-# INLINE at #-}

atPositions :: Foldable t => Error -> t Position -> Error
atPositions GQLError {..} pos = GQLError {locations = locations <> toList pos, ..}
{-# INLINE atPositions #-}

type ValidationError = Error

type ValidationErrors = NonEmpty ValidationError

type Errors = [Error]

msgValidation :: (Msg a) => a -> ValidationError
msgValidation = msg
{-# INLINE msgValidation #-}

manyMsg :: (Foldable t, Msg a) => t a -> Error
manyMsg =
  mkError . Message . T.intercalate ", "
    . fmap (readMessage . message . msg)
    . toList

{-# DEPRECATED InternalError "use Errors" #-}

type InternalError = Error

msgInternal :: (Msg a) => a -> InternalError
msgInternal = msg
{-# INLINE msgInternal #-}

data ErrorType = Internal
  deriving
    ( Show,
      Eq,
      Generic,
      FromJSON,
      ToJSON
    )

instance Semigroup ErrorType where
  Internal <> Internal = Internal

data GQLError = GQLError
  { message :: Message,
    locations :: [Position],
    errorType :: Maybe ErrorType,
    extensions :: Maybe (Map Text Value)
  }
  deriving
    ( Show,
      Eq,
      Generic,
      FromJSON
    )

instance ToJSON GQLError where
  toJSON = genericToJSON (defaultOptions {omitNothingFields = True})

type GQLErrors = NonEmpty GQLError

class Msg a where
  msg :: a -> Error

instance Msg Error where
  msg = id

instance Msg String where
  msg = fromString

instance Msg Message where
  msg = mkError

instance Msg Text where
  msg = mkError . Message

instance Msg ByteString where
  msg = msg . LT.toStrict . decodeUtf8

instance Msg Value where
  msg = msg . encode
