{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Data.Morpheus.Generics.GQLArgs
  ( GQLArgs(..)
  ) where

import qualified Data.Data                         as D
import qualified Data.Morpheus.ErrorMessage        as Err
import           Data.Morpheus.Generics.GDecode    (GDecode (..))
import qualified Data.Morpheus.Generics.GQLInput   as I (GQLInput (..))
import           Data.Morpheus.Generics.TypeRep    (Selectors (..))
import           Data.Morpheus.Types.Introspection (GQLTypeLib, GQL__Deprecation__Args,
                                                    GQL__InputValue)
import           Data.Morpheus.Types.MetaInfo      (MetaInfo (..), initialMeta)
import           Data.Morpheus.Types.Types         (Argument (..), Arguments, Validation)
import           Data.Proxy                        (Proxy (..))
import qualified Data.Text                         as T
import           GHC.Generics

instance (Selector s, D.Typeable t, I.GQLInput t) => Selectors (M1 S s (K1 R t)) GQL__InputValue where
  getFields _ = [(I.typeInfo (Proxy :: Proxy t) name, I.introInput (Proxy :: Proxy t))]
    where
      name = T.pack $ selName (undefined :: M1 S s (K1 R t) ())

instance I.GQLInput a => GDecode Arguments (K1 i a) where
  gDecode meta args =
    case lookup (key meta) args of
      Nothing                -> Left $ Err.requiredArgument meta
      Just (Argument x _pos) -> K1 <$> I.decode x
      Just x                 -> Err.handleError $ T.pack $ show x

class GQLArgs p where
  decode :: Arguments -> Maybe p -> Validation p
  default decode :: (Show p, Generic p, D.Data p, GDecode Arguments (Rep p)) =>
    Arguments -> Maybe p -> Validation p
  decode args _ = to <$> gDecode initialMeta args
  introspect :: Proxy p -> [(GQL__InputValue, GQLTypeLib -> GQLTypeLib)]
  default introspect :: (Show p, Selectors (Rep p) GQL__InputValue, D.Typeable p) =>
    Proxy p -> [(GQL__InputValue, GQLTypeLib -> GQLTypeLib)]
  introspect _ = getFields (Proxy :: Proxy (Rep p))

instance GQLArgs () where
  decode _ _ = pure ()
  introspect _ = []

instance GQLArgs GQL__Deprecation__Args
