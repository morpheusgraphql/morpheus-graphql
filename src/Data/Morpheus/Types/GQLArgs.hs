{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Types.GQLArgs
  ( GQLArgs(..)
  ) where

import           Data.Morpheus.Error.Internal               (internalArgumentError)
import           Data.Morpheus.Generics.GDecode             (GDecode (..))
import           Data.Morpheus.Generics.ObjectRep           (ObjectRep (..))
import           Data.Morpheus.Generics.Utils               (RecSel, SelOf)
import           Data.Morpheus.Kind.Decode                  (InputTypeRouter, _decode, _field, _introspect)
import           Data.Morpheus.Kind.Internal                (KIND)
import           Data.Morpheus.Schema.Type                  (DeprecationArgs)
import           Data.Morpheus.Types.Internal.AST.Selection (Argument (..), Arguments)
import           Data.Morpheus.Types.Internal.Data          (DataInputField, DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (Validation)
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

instance (Selector s, InputTypeRouter a (KIND a)) => ObjectRep (RecSel s a) (Text, DataInputField) where
  getFields _ = [((name, _field (Proxy @a) name), _introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: SelOf s)

instance InputTypeRouter a (KIND a) => GDecode Arguments (K1 i a) where
  gDecode key' args =
    case lookup key' args of
      Nothing                -> internalArgumentError "Required Argument Not Found"
      Just (Argument x _pos) -> K1 <$> _decode x

class GQLArgs p where
  decode :: Arguments -> Validation p
  default decode :: (Generic p, GDecode Arguments (Rep p)) =>
    Arguments -> Validation p
  decode args = to <$> gDecode "" args
  introspect :: Proxy p -> [((Text, DataInputField), DataTypeLib -> DataTypeLib)]
  default introspect :: ObjectRep (Rep p) (Text, DataInputField) =>
    Proxy p -> [((Text, DataInputField), DataTypeLib -> DataTypeLib)]
  introspect _ = getFields (Proxy @(Rep p))

instance GQLArgs () where
  decode _ = pure ()
  introspect _ = []

instance GQLArgs DeprecationArgs
