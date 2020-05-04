{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Server.Deriving.Generics.EnumRep
  ( EnumRep (..),
  )
where

import Data.Proxy (Proxy (..))
import Data.Text
  ( Text,
    pack,
  )
import GHC.Generics

-- MORPHEUS
class EnumRep (f :: * -> *) where
  enumTags :: Proxy f -> [Text]

instance EnumRep f => EnumRep (M1 D c f) where
  enumTags _ = enumTags (Proxy @f)

instance (Constructor c) => EnumRep (M1 C c U1) where
  enumTags _ = [pack $ conName (undefined :: (M1 C c U1 x))]

instance (EnumRep a, EnumRep b) => EnumRep (a :+: b) where
  enumTags _ = enumTags (Proxy @a) ++ enumTags (Proxy @b)
