{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Error
  ( at,
    atPositions,
    ValidationError,
    ValidationErrors,
    toGQLError,
    Error,
    GQLError (..),
    GQLErrors,
    readErrorMessage,
    mapError,
    msgValidation,
    msgInternal,
    InternalError,
    manyMsg,
  )
where

import Data.Aeson
  ( FromJSON,
    Options (..),
    ToJSON (..),
    Value,
    defaultOptions,
    genericToJSON,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Message (..),
    Msg (..),
    Position (..),
  )
import qualified Data.Text as T
import Relude

data ERROR = VALIDATION | INTERNAL

readErrorMessage :: Error a -> Message
readErrorMessage = errorMessage

data Error (a :: ERROR) = Error
  { errorMessage :: Message,
    errorLocations :: [Position]
  }
  deriving (Show)

instance IsString (Error t) where
  fromString = (`Error` []) . msg

instance Semigroup (Error t) where
  Error m1 p1 <> Error m2 p2 = Error (m1 <> m2) (p1 <> p2)

at :: Error t -> Position -> Error t
at err pos = atPositions err [pos]
{-# INLINE at #-}

atPositions :: Foldable t => Error a -> t Position -> Error a
atPositions (Error m ps) pos = Error m (ps <> toList pos)
{-# INLINE atPositions #-}

type ValidationError = Error 'VALIDATION

type ValidationErrors = [ValidationError]

toGQLError :: Error t -> GQLError
toGQLError (Error m p) = GQLError m p Nothing
{-# INLINE toGQLError #-}

msgValidation :: (Msg a) => a -> ValidationError
msgValidation = (`Error` []) . msg
{-# INLINE msgValidation #-}

manyMsg :: (Foldable t, Msg a) => t a -> Error k
manyMsg = (`Error` []) . Message . T.intercalate ", " . fmap (readMessage . msg) . toList

mapError :: (Message -> Message) -> ValidationError -> GQLError
mapError f (Error text locations) =
  GQLError
    { message = f text,
      locations,
      extensions = Nothing
    }

type InternalError = Error 'INTERNAL

msgInternal :: (Msg a) => a -> InternalError
msgInternal = (`Error` []) . msg
{-# INLINE msgInternal #-}

instance Msg (Error 'INTERNAL) where
  msg = ("Internal Error! " <>) . errorMessage

data GQLError = GQLError
  { message :: Message,
    locations :: [Position],
    extensions :: Maybe Value
  }
  deriving
    ( Show,
      Eq,
      Generic,
      FromJSON
    )

instance ToJSON GQLError where
  toJSON = genericToJSON (defaultOptions {omitNothingFields = True})

type GQLErrors = [GQLError]
