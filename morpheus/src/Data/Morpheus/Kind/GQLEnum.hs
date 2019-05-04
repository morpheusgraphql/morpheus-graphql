{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}

module Data.Morpheus.Kind.GQLEnum
  ( GQLEnum(..)
  ) where

import           Data.Morpheus.Generics.GDecodeEnum     (GDecodeEnum (..))
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..), enumTypeOf)
import           Data.Morpheus.Kind.Internal            (ENUM, GQL, IntrospectionRouter (..), _decode, _field,
                                                         _introspect, _objectField)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.Internal.Types    (Field (..), InputField (..), ObjectField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import           GHC.Generics

class GQLEnum a where
  decode :: Text -> a
  default decode :: (Generic a, GDecodeEnum (Rep a)) =>
    Text -> a
  decode text = to $ gToEnum text
  asInputField :: Proxy a -> Text -> InputField
  default asInputField :: GQLKind a =>
    Proxy a -> Text -> InputField
  asInputField proxy = InputField . asField proxy
  asField :: Proxy a -> Text -> Field
  default asField :: GQLKind a =>
    Proxy a -> Text -> Field
  asField proxy name = Field {fieldName = name, notNull = True, kind = ENUM, fieldType = typeID proxy, asList = False}
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: (GQLKind a, GDecodeEnum (Rep a)) =>
    Proxy a -> TypeLib -> TypeLib
  introspect = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

instance GQLEnum TypeKind

instance GQLEnum DirectiveLocation

type instance GQL TypeKind = ENUM

type instance GQL DirectiveLocation = ENUM

setNullable :: Field -> Field
setNullable x = x {notNull = False}

wrapMaybe :: InputField -> InputField
wrapMaybe = InputField . setNullable . unpackInputField

instance (GQLEnum a, GQLKind a) => GQLEnum (Maybe a) where
  decode x = Just (decode x)
  asInputField _ name = wrapMaybe $ asInputField (Proxy @a) name
  introspect _ = introspect (Proxy @a)

instance (GQLEnum a, GQLKind a) => GQLEnum [a] where
  decode x = [decode x]
  introspect _ = introspect (Proxy @a)
