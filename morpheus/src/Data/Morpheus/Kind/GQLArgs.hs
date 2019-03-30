{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Morpheus.Kind.GQLArgs
  ( GQLArgs(..)
  ) where

import qualified Data.Data                           as D
import           Data.Morpheus.Error.Internal        (internalArgumentError)
import           Data.Morpheus.Generics.GDecode      (GDecode (..))
import           Data.Morpheus.Generics.TypeRep      (Selectors (..))
import           Data.Morpheus.Generics.Utils        (RecSel, SelOf)
import qualified Data.Morpheus.Kind.GQLInput         as I (GQLInput (..))
import           Data.Morpheus.Kind.GQLKind          (GQLKind)
import           Data.Morpheus.Schema.Internal.Types (InputField, TypeLib)
import           Data.Morpheus.Schema.Type           (DeprecationArgs)
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), initialMeta)
import           Data.Morpheus.Types.Query.Selection (Argument (..), Arguments)
import           Data.Proxy                          (Proxy (..))
import qualified Data.Text                           as T
import           GHC.Generics

instance (Selector s, D.Typeable a, GQLKind a, I.GQLInput a) => Selectors (RecSel s a) InputField where
  getFields _ = [(I.asArgument (Proxy @a) name, I.introInput (Proxy @a))]
    where
      name = T.pack $ selName (undefined :: SelOf s)

instance I.GQLInput a => GDecode Arguments (K1 i a) where
  gDecode meta args =
    case lookup (key meta) args of
      Nothing                -> internalArgumentError "Required Argument Not Found"
      Just (Argument x _pos) -> K1 <$> I.decode x

class GQLArgs p where
  decode :: Arguments -> Validation p
  default decode :: (Generic p, GDecode Arguments (Rep p)) =>
    Arguments -> Validation p
  decode args = to <$> gDecode initialMeta args
  introspect :: Proxy p -> [(InputField, TypeLib -> TypeLib)]
  default introspect :: Selectors (Rep p) InputField =>
    Proxy p -> [(InputField, TypeLib -> TypeLib)]
  introspect _ = getFields (Proxy @(Rep p))

instance GQLArgs () where
  decode _ = pure ()
  introspect _ = []

instance GQLArgs DeprecationArgs
