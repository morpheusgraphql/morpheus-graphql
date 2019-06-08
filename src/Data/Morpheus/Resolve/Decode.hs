{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Resolve.Decode where

import           Data.Morpheus.Error.Internal           (internalArgumentError, internalTypeMismatch)
import           Data.Morpheus.Kind                     (ENUM, INPUT_OBJECT, KIND, SCALAR, WRAPPER)
import           Data.Morpheus.Resolve.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Resolve.Generics.GDecode (GDecode (..))
import           Data.Morpheus.Resolve.Generics.TypeRep (ObjectRep (..), RecSel, SelOf)
import           Data.Morpheus.Resolve.Internal         (Decode_, EnumConstraint, IField_, InputObjectConstraint,
                                                         Intro_, introspectEnum, introspectInputObject, listField,
                                                         maybeField)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import qualified Data.Morpheus.Types.GQLScalar          as S (GQLScalar (..))
import           Data.Morpheus.Types.GQLType            (GQLType, field_)
import           Data.Morpheus.Types.Internal.Data      (DataInputField)
import           Data.Morpheus.Types.Internal.Value     (Value (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text, pack)
import           GHC.Generics

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

class InputTypeRouter a b where
  __introspect :: Proxy b -> Intro_ a
  __decode :: Proxy b -> Decode_ a
  __field :: Proxy b -> IField_ a

instance (InputTypeRouter a (KIND a)) => GDecode Value (K1 i a) where
  gDecode key' (Object object) =
    case lookup key' object of
      Nothing    -> internalArgumentError "Missing Argument"
      Just value -> K1 <$> _decode value
  gDecode _ isType = internalTypeMismatch "InputObject" isType

instance (S.GQLScalar a, GQLType a) => InputTypeRouter a SCALAR where
  __decode _ = S.decode
  __introspect _ _ = S.introspect (Proxy @a)
  __field _ _ = field_ SCALAR (Proxy @a) ()

instance EnumConstraint a => InputTypeRouter a ENUM where
  __decode _ (Enum value) = pure (to $ gToEnum value)
  __decode _ isType       = internalTypeMismatch "Enum" isType
  __field _ _ = field_ ENUM (Proxy @a) ()
  __introspect _ _ = introspectEnum (Proxy @a)

instance InputObjectConstraint a => InputTypeRouter a INPUT_OBJECT where
  __decode _ (Object x) = to <$> gDecode "" (Object x)
  __decode _ isType     = internalTypeMismatch "InputObject" isType
  __field _ _ = field_ INPUT_OBJECT (Proxy @a) ()
  __introspect _ = introspectInputObject

instance InputTypeRouter a (KIND a) => InputTypeRouter (Maybe a) WRAPPER where
  __decode _ Null = pure Nothing
  __decode _ x    = Just <$> _decode x
  __field _ _ name = maybeField $ _field (Proxy @a) name
  __introspect _ _ = _introspect (Proxy @a)

instance InputTypeRouter a (KIND a) => InputTypeRouter [a] WRAPPER where
  __decode _ (List li) = mapM _decode li
  __decode _ isType    = internalTypeMismatch "List" isType
  __field _ _ name = listField $ _field (Proxy @a) name
  __introspect _ _ = _introspect (Proxy @a)

instance (Selector s, InputTypeRouter a (KIND a)) => ObjectRep (RecSel s a) (Text, DataInputField) where
  getFields _ = [((name, _field (Proxy @a) name), _introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: SelOf s)
