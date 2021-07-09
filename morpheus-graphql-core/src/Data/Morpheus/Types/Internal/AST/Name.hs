{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Name
  ( Name (..),
    FieldName,
    TypeName,
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
    fromText,
    renderGQL,
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

data NAME
  = TYPE
  | FIELD

newtype Name (t :: NAME) = Name {unName :: Text}
  deriving
    (Generic)
  deriving newtype
    ( Show,
      Ord,
      Eq,
      IsString,
      ToString,
      Hashable,
      Semigroup,
      FromJSON,
      ToJSON
    )

liftTypedString :: IsString a => Text -> Q (TExp a)
liftTypedString = unsafeTExpCoerce . stringE . T.unpack
{-# INLINE liftTypedString #-}

liftString :: Text -> ExpQ
liftString = stringE . T.unpack
{-# INLINE liftString #-}

instance Lift (Name t) where
  lift = liftString . unName

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedString . unName
#endif

instance RenderGQL (Name a) where
  renderGQL = fromText . unName

type FieldName = Name 'FIELD

type TypeName = Name 'TYPE
