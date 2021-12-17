{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Error
  ( at,
    atPositions,
    internal,
    isInternal,
    GQLErrors,
    GQLError
      ( message,
        locations
      ),
    manyMsg,
    Msg (..),
    Message,
    withPath,
    withExtensions,
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
  ( Position (..),
  )
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Relude hiding (ByteString, decodeUtf8)

type Message = Text

internal :: GQLError -> GQLError
internal x = x {errorType = Just Internal}

isInternal :: GQLError -> Bool
isInternal GQLError {errorType = Just Internal} = True
isInternal _ = False

at :: GQLError -> Position -> GQLError
at err pos = atPositions err [pos]
{-# INLINE at #-}

atPositions :: Foldable t => GQLError -> t Position -> GQLError
atPositions GQLError {..} pos = case toList pos of
  [] -> GQLError {..}
  posList -> GQLError {locations = locations <> Just posList, ..}
{-# INLINE atPositions #-}


withExtensions :: GQLError -> Map Text Value -> GQLError
withExtensions gqlerr ext = gqlerr { extensions = Just ext }

withPath :: GQLError -> [Text] -> GQLError
withPath err [] = err
withPath err path = err {path = Just path}

manyMsg :: (Foldable t, Msg a) => t a -> GQLError
manyMsg =
  msg . T.intercalate ", "
    . fmap (message . msg)
    . toList

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
    locations :: Maybe [Position],
    path :: Maybe [Text],
    errorType :: Maybe ErrorType,
    extensions :: Maybe (Map Text Value)
  }
  deriving
    ( Show,
      Eq,
      Generic,
      FromJSON
    )

instance Ord GQLError where
  compare x y = compare (locations x) (locations y) <> compare (message x) (message y)

instance IsString GQLError where
  fromString = msg

instance ToJSON GQLError where
  toJSON = genericToJSON (defaultOptions {omitNothingFields = True})

instance Semigroup GQLError where
  GQLError m1 l1 p1 t1 e1 <> GQLError m2 l2 p2 t2 e2 = GQLError (m1 <> m2) (l1 <> l2) (p1 <> p2) (t1 <> t2) (e1 <> e2)

type GQLErrors = NonEmpty GQLError

class Msg a where
  msg :: a -> GQLError

instance Msg GQLError where
  msg = id

instance Msg String where
  msg = msg . T.pack

instance Msg Text where
  msg message =
    GQLError
      { message,
        locations = Nothing,
        errorType = Nothing,
        extensions = Nothing,
        path = Nothing
      }

instance Msg ByteString where
  msg = msg . LT.toStrict . decodeUtf8

instance Msg Value where
  msg = msg . encode
