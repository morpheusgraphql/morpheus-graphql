{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Morpheus.Generics.GQLArgs
  ( GQLArgs(..)
  ) where

import qualified Data.Data                        as D
import           Data.Morpheus.Error.Arguments    (requiredArgument)
import           Data.Morpheus.Error.Error        (handleError)
import           Data.Morpheus.Generics.GDecode   (GDecode (..))
import qualified Data.Morpheus.Generics.GQLInput  as I (GQLInput (..))
import           Data.Morpheus.Generics.TypeRep   (Selectors (..))
import           Data.Morpheus.Generics.Utils     (RecSel, SelOf)
import           Data.Morpheus.Schema.Type        (DeprecationArgs)
import           Data.Morpheus.Schema.Utils.Utils (InputValue, TypeLib)
import           Data.Morpheus.Types.Error        (Validation)
import           Data.Morpheus.Types.MetaInfo     (MetaInfo (..), initialMeta)
import           Data.Morpheus.Types.Types        (Argument (..), Arguments)

import           Data.Proxy                       (Proxy (..))
import qualified Data.Text                        as T
import           GHC.Generics

instance (Selector s, D.Typeable a, I.GQLInput a) => Selectors (RecSel s a) InputValue where
  getFields _ = [(I.typeInfo (Proxy @a) name, I.introInput (Proxy @a))]
    where
      name = T.pack $ selName (undefined :: SelOf s)

instance I.GQLInput a => GDecode Arguments (K1 i a) where
  gDecode meta args =
    case lookup (key meta) args
      -- TODO: validate it in PreProcess
          of
      Nothing                -> Left $ requiredArgument meta
      Just (Argument x _pos) -> K1 <$> I.decode x
      Just x                 -> handleError $ T.pack $ show x

class GQLArgs p where
  decode :: Arguments -> Maybe p -> Validation p
  default decode :: (Show p, Generic p, D.Data p, GDecode Arguments (Rep p)) =>
    Arguments -> Maybe p -> Validation p
  decode args _ = to <$> gDecode initialMeta args
  introspect :: Proxy p -> [(InputValue, TypeLib -> TypeLib)]
  default introspect :: (Show p, Selectors (Rep p) InputValue, D.Typeable p) =>
    Proxy p -> [(InputValue, TypeLib -> TypeLib)]
  introspect _ = getFields (Proxy @(Rep p))

instance GQLArgs () where
  decode _ _ = pure ()
  introspect _ = []

instance GQLArgs DeprecationArgs
