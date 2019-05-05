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
import qualified Data.Morpheus.Kind.GQLEnum        as E (EnumConstraint, GQLEnum (..))
import qualified Data.Morpheus.Kind.GQLInputObject as I (GQLInputObject (..))
import           Data.Morpheus.Kind.GQLKind        (GQLKind)
import qualified Data.Morpheus.Kind.GQLScalar      as S (GQLScalar (..))
import           Data.Morpheus.Kind.Internal       (Decode_, ENUM, GQL, GQLConstraint, IField_, INPUT_OBJECT, Intro_,
                                                    SCALAR, WRAPPER)
import           Data.Morpheus.Kind.Utils          (listInputField, maybeInputField)
import           Data.Morpheus.Types.JSType        (JSType (..))
import           Data.Morpheus.Types.MetaInfo      (MetaInfo (..))
import           Data.Proxy                        (Proxy (..))
import           GHC.Generics

class InputTypeRouter a b where
  __introspect :: Proxy b -> Intro_ a
  __decode :: Proxy b -> Decode_ a
  __field :: Proxy b -> IField_ a

_field ::
     forall a. InputTypeRouter a (GQL a)
  => IField_ a
_field = __field (Proxy @(GQL a))

_decode ::
     forall a. InputTypeRouter a (GQL a)
  => Decode_ a
_decode = __decode (Proxy @(GQL a))

_introspect ::
     forall a. InputTypeRouter a (GQL a)
  => Intro_ a
_introspect = __introspect (Proxy @(GQL a))

type instance GQLConstraint a SCALAR = S.GQLScalar a

type instance GQLConstraint a ENUM = E.GQLEnum a

type instance GQLConstraint a INPUT_OBJECT = I.GQLInputObject a

instance (InputTypeRouter a (GQL a)) => GDecode JSType (K1 i a) where
  gDecode meta (JSObject object) =
    case lookup (key meta) object of
      Nothing    -> internalArgumentError "Missing Argument"
      Just value -> K1 <$> _decode value
  gDecode _ isType = internalTypeMismatch "InputObject" isType

instance (S.GQLScalar a, GQLKind a) => InputTypeRouter a SCALAR where
  __decode _ = S.decode
  __introspect _ _ = S.introspect (Proxy @a)
  __field _ _ = S.asInputField (Proxy @a)

instance E.EnumConstraint a => InputTypeRouter a ENUM where
  __decode _ (JSEnum value) = pure (E.decode value)
  __decode _ isType         = internalTypeMismatch "Enum" isType
  __introspect _ _ = E.introspect (Proxy @a)
  __field _ _ = E.inputField (Proxy @a)

instance (I.GQLInputObject a, GQLKind a) => InputTypeRouter a INPUT_OBJECT where
  __decode _ = I.decode
  __introspect _ = I.introInput
  __field _ = I.asArgument

instance InputTypeRouter a (GQL a) => InputTypeRouter (Maybe a) WRAPPER where
  __decode _ JSNull = pure Nothing
  __decode _ x      = Just <$> _decode x
  __field _ _ name = maybeInputField $ _field (Proxy @a) name
  __introspect _ _ = _introspect (Proxy @a)

instance InputTypeRouter a (GQL a) => InputTypeRouter [a] WRAPPER where
  __decode _ (JSList li) = mapM _decode li
  __decode _ isType      = internalTypeMismatch "List" isType
  __field _ _ name = listInputField $ _field (Proxy @a) name
  __introspect _ _ = _introspect (Proxy @a)
