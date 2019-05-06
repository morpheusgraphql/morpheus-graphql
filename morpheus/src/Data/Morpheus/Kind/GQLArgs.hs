{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.GQLArgs
  ( GQLArgs(..)
  ) where

import           Data.Morpheus.Error.Internal        (internalArgumentError)
import           Data.Morpheus.Generics.GDecode      (GDecode (..))
import           Data.Morpheus.Generics.TypeRep      (Selectors (..))
import           Data.Morpheus.Generics.Utils        (RecSel, SelOf)
import           Data.Morpheus.Kind.InputRouter      (InputTypeRouter, _decode, _field, _introspect)
import           Data.Morpheus.Kind.Internal         (GQL)
import           Data.Morpheus.Schema.Internal.Types (InputField, TypeLib)
import           Data.Morpheus.Schema.Type           (DeprecationArgs)
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.Query.Selection (Argument (..), Arguments)
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (Text, pack)
import           GHC.Generics

instance (Selector s, InputTypeRouter a (GQL a)) => Selectors (RecSel s a) (Text, InputField) where
  getFields _ = [((name, _field (Proxy @a) name), _introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: SelOf s)

instance InputTypeRouter a (GQL a) => GDecode Arguments (K1 i a) where
  gDecode key' args =
    case lookup key' args of
      Nothing                -> internalArgumentError "Required Argument Not Found"
      Just (Argument x _pos) -> K1 <$> _decode x

class GQLArgs p where
  decode :: Arguments -> Validation p
  default decode :: (Generic p, GDecode Arguments (Rep p)) =>
    Arguments -> Validation p
  decode args = to <$> gDecode "" args
  introspect :: Proxy p -> [((Text, InputField), TypeLib -> TypeLib)]
  default introspect :: Selectors (Rep p) (Text, InputField) =>
    Proxy p -> [((Text, InputField), TypeLib -> TypeLib)]
  introspect _ = getFields (Proxy @(Rep p))

instance GQLArgs () where
  decode _ = pure ()
  introspect _ = []

instance GQLArgs DeprecationArgs
