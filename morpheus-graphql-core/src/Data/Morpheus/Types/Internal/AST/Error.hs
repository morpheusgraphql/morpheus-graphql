{-# LANGUAGE DataKinds #-}
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
    custom,
    isCustom,
    internal,
    isInternal,
    getCustomErrorType,
    GQLErrors,
    GQLError
      ( message,
        locations,
        path
      ),
    manyMsg,
    Msg (..),
    Message,
    withPath,
    withExtensions,
    PropName (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    Value (Null, Number, String),
    defaultOptions,
    encode,
    genericParseJSON,
    genericToJSON,
  )
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Types.Internal.AST.Base
  ( Position (..),
  )
import Data.Scientific (floatingOrInteger)
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

custom :: GQLError -> Text -> GQLError
custom x customError = x {errorType = Just (Custom customError)}

isCustom :: GQLError -> Bool
isCustom GQLError {errorType = Just (Custom _)} = True
isCustom _ = False

getCustomErrorType :: GQLError -> Maybe Text
getCustomErrorType GQLError {errorType = Just (Custom customError)} = Just customError
getCustomErrorType _ = Nothing

at :: GQLError -> Position -> GQLError
at err pos = atPositions err [pos]
{-# INLINE at #-}

atPositions :: (Foldable t) => GQLError -> t Position -> GQLError
atPositions GQLError {..} pos = case toList pos of
  [] -> GQLError {..}
  posList -> GQLError {locations = locations <> Just posList, ..}
{-# INLINE atPositions #-}

withExtensions :: GQLError -> Map Text Value -> GQLError
withExtensions gqlErr ext = gqlErr {extensions = Just ext}

withPath :: GQLError -> [PropName] -> GQLError
withPath err [] = err
withPath err path = err {path = Just path}

manyMsg :: (Foldable t, Msg a) => t a -> GQLError
manyMsg =
  msg
    . T.intercalate ", "
    . fmap (message . msg)
    . toList

data ErrorType
  = Internal
  | Custom Text
  deriving
    ( Show,
      Eq,
      Generic
    )

instance ToJSON ErrorType where
  toJSON (Custom customError) = String customError
  toJSON Internal = Null

instance FromJSON ErrorType where
  parseJSON (String customError) = pure $ Custom customError
  parseJSON _ = fail "Unexpected custom error type"

instance Semigroup ErrorType where
  Internal <> _ = Internal
  _ <> Internal = Internal
  Custom customError <> Custom customError' = Custom $ customError <> ", " <> customError'

data GQLError = GQLError
  { message :: Message,
    locations :: Maybe [Position],
    path :: Maybe [PropName],
    errorType :: Maybe ErrorType,
    extensions :: Maybe (Map Text Value)
  }
  deriving
    ( Show,
      Eq,
      Generic
    )

data PropName
  = PropIndex Int
  | PropName Text
  deriving (Show, Eq, Generic)

instance IsString PropName where
  fromString = PropName . T.pack

instance FromJSON PropName where
  parseJSON (String name) = pure (PropName name)
  parseJSON (Number v) = case floatingOrInteger v of
    Left fl -> invalidIndex fl
    Right index -> pure (PropIndex index)
  parseJSON _ = fail "Property Name must be a either Name or Index"

invalidIndex :: (MonadFail m) => Double -> m a
invalidIndex i = fail $ "Property Name must be a either Name or Index. it can't be " <> show i <> "."

instance ToJSON PropName where
  toJSON (PropName name) = toJSON name
  toJSON (PropIndex index) = toJSON index

instance Ord GQLError where
  compare x y = compare (locations x) (locations y) <> compare (message x) (message y)

instance IsString GQLError where
  fromString = msg

-- cannot have 'type' as the record name, this is less painful than
-- manually writing to/from JSON instances
stripErrorPrefix :: String -> String
stripErrorPrefix "errorType" = "type"
stripErrorPrefix other = other

aesonOptions :: Options
aesonOptions = defaultOptions {omitNothingFields = True, fieldLabelModifier = stripErrorPrefix}

instance ToJSON GQLError where
  toJSON = genericToJSON aesonOptions

instance FromJSON GQLError where
  parseJSON = genericParseJSON aesonOptions

instance Semigroup GQLError where
  GQLError m1 l1 p1 t1 e1 <> GQLError m2 l2 p2 t2 e2 = GQLError (m1 <> m2) (l1 <> l2) (p1 <> p2) (t1 <> t2) (e1 <> e2)

type GQLErrors = NonEmpty GQLError

class Msg a where
  msg :: a -> GQLError

instance Msg GQLError where
  msg = id

instance Msg String where
  msg = msg . T.pack

instance Msg Int where
  msg = msg . T.pack . show

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
