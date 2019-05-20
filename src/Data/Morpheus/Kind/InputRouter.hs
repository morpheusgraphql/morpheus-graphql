{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.InputRouter where

import           Data.Morpheus.Error.Internal      (internalArgumentError, internalTypeMismatch)
import           Data.Morpheus.Generics.GDecode    (GDecode (..))
import qualified Data.Morpheus.Kind.GQLEnum        as E (EnumConstraint, decode, introspect)
import qualified Data.Morpheus.Kind.GQLInputObject as I (IObjectConstraint, decode, inputField, introspect)
import qualified Data.Morpheus.Kind.GQLScalar      as S (GQLScalar (..))
import           Data.Morpheus.Kind.GQLType        (GQLType, field_)
import           Data.Morpheus.Kind.Internal       (Decode_, ENUM, IField_, INPUT_OBJECT, Intro_, KIND, SCALAR, WRAPPER)
import           Data.Morpheus.Kind.Utils          (listField, maybeField)
import           Data.Morpheus.Schema.TypeKind     (TypeKind (..))
import           Data.Morpheus.Types.JSType        (JSType (..))
import           Data.Proxy                        (Proxy (..))
import           GHC.Generics

class InputTypeRouter a b where
  __introspect :: Proxy b -> Intro_ a
  __decode :: Proxy b -> Decode_ a
  __field :: Proxy b -> IField_ a

_field ::
     forall a. InputTypeRouter a (KIND a)
  => IField_ a
_field = __field (Proxy @(KIND a))

_decode ::
     forall a. InputTypeRouter a (KIND a)
  => Decode_ a
_decode = __decode (Proxy @(KIND a))

_introspect ::
     forall a. InputTypeRouter a (KIND a)
  => Intro_ a
_introspect = __introspect (Proxy @(KIND a))

instance (InputTypeRouter a (KIND a)) => GDecode JSType (K1 i a) where
  gDecode key' (JSObject object) =
    case lookup key' object of
      Nothing    -> internalArgumentError "Missing Argument"
      Just value -> K1 <$> _decode value
  gDecode _ isType = internalTypeMismatch "InputObject" isType

instance (S.GQLScalar a, GQLType a) => InputTypeRouter a SCALAR where
  __decode _ = S.decode
  __introspect _ _ = S.introspect (Proxy @a)
  __field _ _ = field_ SCALAR (Proxy @a) ()

instance E.EnumConstraint a => InputTypeRouter a ENUM where
  __decode _ (JSEnum value) = pure (E.decode value)
  __decode _ isType         = internalTypeMismatch "Enum" isType
  __introspect _ _ = E.introspect (Proxy @a)
  __field _ _ = field_ ENUM (Proxy @a) ()

instance I.IObjectConstraint a => InputTypeRouter a INPUT_OBJECT where
  __decode _ = I.decode
  __field _ = I.inputField
  __introspect _ = I.introspect

instance InputTypeRouter a (KIND a) => InputTypeRouter (Maybe a) WRAPPER where
  __decode _ JSNull = pure Nothing
  __decode _ x      = Just <$> _decode x
  __field _ _ name = maybeField $ _field (Proxy @a) name
  __introspect _ _ = _introspect (Proxy @a)

instance InputTypeRouter a (KIND a) => InputTypeRouter [a] WRAPPER where
  __decode _ (JSList li) = mapM _decode li
  __decode _ isType      = internalTypeMismatch "List" isType
  __field _ _ name = listField $ _field (Proxy @a) name
  __introspect _ _ = _introspect (Proxy @a)
