{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Base
  ( Ref (..),
    Position (..),
    Message (..),
    Description,
    OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    Token,
    toOperationType,
    GQLError (..),
    GQLErrors,
    TRUE,
    FALSE,
    Msg (..),
    InternalError (..),
    msgInternal,
    ValidationError (..),
    msgValidation,
    ValidationErrors,
    withPosition,
    toGQLError,
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
import Data.Char (toLower)
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    renderGQL,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( Name,
    TypeName,
    unpackName,
  )
import Data.Text (intercalate, pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Language.Haskell.TH
  ( ExpQ,
    stringE,
  )
import Language.Haskell.TH.Syntax
  ( Lift (..),
    Q,
    TExp,
    unsafeTExpCoerce,
  )
import Relude hiding
  ( ByteString,
    decodeUtf8,
    intercalate,
  )

type TRUE = 'True

type FALSE = 'False

-- Strings
type Token = Text

-- Error / Warning Messages
newtype Message = Message {readMessage :: Text}
  deriving
    (Generic)
  deriving newtype
    (Show, Eq, Ord, IsString, Semigroup, Hashable, FromJSON, ToJSON)

instance Lift Message where
  lift = liftString . readMessage

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedString . readMessage
#endif

newtype InternalError = InternalError
  { readInternalError :: Text
  }
  deriving
    (Generic)
  deriving newtype
    (Show, Eq, Ord, IsString, Semigroup, Hashable, FromJSON, ToJSON)

data ValidationError = ValidationError
  { validationMessage :: Message,
    validationLocations :: [Position]
  }
  deriving (Show)

instance IsString ValidationError where
  fromString = (`ValidationError` []) . msg

instance Semigroup ValidationError where
  ValidationError m1 p1 <> ValidationError m2 p2 =
    ValidationError (m1 <> m2) (p1 <> p2)

withPosition :: Maybe Position -> ValidationError -> ValidationError
withPosition pos (ValidationError m ps) = ValidationError m (ps <> maybeToList pos)
{-# INLINE withPosition #-}

type ValidationErrors = [ValidationError]

toGQLError :: ValidationError -> GQLError
toGQLError (ValidationError m p) = GQLError m p Nothing
{-# INLINE toGQLError #-}

msgInternal :: (Msg a) => a -> InternalError
msgInternal = InternalError . readMessage . msg
{-# INLINE msgInternal #-}

msgValidation :: (Msg a) => a -> ValidationError
msgValidation = (`ValidationError` []) . msg
{-# INLINE msgValidation #-}

class Msg a where
  msg :: a -> Message
  msgSepBy :: Text -> [a] -> Message
  msgSepBy t = Message . intercalate t . fmap (readMessage . msg)

instance Msg Message where
  msg = id

instance Msg InternalError where
  msg = Message . ("Internal Error! " <>) . readInternalError

instance Msg String where
  msg = Message . pack

instance Msg ByteString where
  msg = Message . LT.toStrict . decodeUtf8

instance Msg Text where
  msg = Message

instance Msg Value where
  msg = msg . encode

instance Msg (Name t) where
  msg name = Message $ "\"" <> unpackName name <> "\""

liftTypedString :: IsString a => Token -> Q (TExp a)
liftTypedString = unsafeTExpCoerce . stringE . T.unpack
{-# INLINE liftTypedString #-}

liftString :: Token -> ExpQ
liftString = stringE . T.unpack
{-# INLINE liftString #-}

-- Description
type Description = Text

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON, Lift)

instance Ord Position where
  compare x y = compare (line x) (line y)

-- Positions 2 Value with same structure
-- but different Positions should be Equal
instance Eq Position where
  _ == _ = True

data GQLError = GQLError
  { message :: Message,
    locations :: [Position],
    extensions :: Maybe Value
  }
  deriving (Show, Eq, Generic, FromJSON)

instance ToJSON GQLError where
  toJSON = genericToJSON (defaultOptions {omitNothingFields = True})

type GQLErrors = [GQLError]

data OperationType
  = Query
  | Subscription
  | Mutation
  deriving (Show, Eq, Lift, Generic, Hashable)

instance RenderGQL OperationType where
  renderGQL = fromString . fmap toLower . show

instance Msg OperationType where
  msg Query = msg ("query" :: TypeName)
  msg Mutation = msg ("mutation" :: TypeName)
  msg Subscription = msg ("subscription" :: TypeName)

type QUERY = 'Query

type MUTATION = 'Mutation

type SUBSCRIPTION = 'Subscription

-- | Document Reference with its Position
--
-- Position is used only for error messages. that means:
--
-- Ref "a" 1 === Ref "a" 3
data Ref name = Ref
  { refName :: name,
    refPosition :: Position
  }
  deriving (Show, Lift, Eq)

instance Ord name => Ord (Ref name) where
  compare (Ref x _) (Ref y _) = compare x y

toOperationType :: TypeName -> Maybe OperationType
toOperationType "Subscription" = Just Subscription
toOperationType "Mutation" = Just Mutation
toOperationType "Query" = Just Query
toOperationType _ = Nothing
{-# INLINE toOperationType #-}
