{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Morpheus.Kind.GQLEnum
  ( GQLEnum(decode, introspect, enumType, fieldType)
  ) where

import           Data.Morpheus.Generics.GDecodeEnum     (GDecodeEnum (..))
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..), enumTypeOf)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.Field             (createFieldWith)
import           Data.Morpheus.Schema.InputValue        (createInputValueWith)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, TypeLib)
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           GHC.Generics

class GQLEnum a where
  decode :: Text -> a
  default decode :: (Show a, Generic a, GDecodeEnum (Rep a)) =>
    Text -> a
  decode text = to $ gToEnum text
  enumType :: Proxy a -> T.Text -> InputValue
  default enumType :: GQLKind a =>
    Proxy a -> T.Text -> InputValue
  enumType proxy name = createInputValueWith name (enumTypeOf [] proxy)
  fieldType :: Proxy a -> T.Text -> Field
  default fieldType :: GQLKind a =>
    Proxy a -> T.Text -> Field
  fieldType proxy name = createFieldWith name (enumTypeOf [] proxy) []
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: (GQLKind a, GDecodeEnum (Rep a)) =>
    Proxy a -> TypeLib -> TypeLib
  introspect = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

instance GQLEnum TypeKind

instance GQLEnum DirectiveLocation
