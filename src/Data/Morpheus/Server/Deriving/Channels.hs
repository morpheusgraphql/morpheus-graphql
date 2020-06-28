{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Deriving.Channels
  ( GetChannels (..),
  )
where

-- MORPHEUS

import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( SubscriptionField (..),
  )
import Data.Semigroup ((<>))
import Data.Text
  ( pack,
  )
import GHC.Generics

class (Generic a, TypeRep (Rep a)) => GetChannels a where
  getChannels :: a -> [(FieldName, String)]

instance {-# OVERLAPPABLE #-} (Generic a, TypeRep (Rep a)) => GetChannels a where
  getChannels = typeRep . from

class GetChannel a where
  getChannel :: a -> String

instance GetChannel (SubscriptionField a) where
  getChannel SubscriptionField {channel} = channel

class TypeRep f where
  typeRep :: f a -> [(FieldName, String)]

instance TypeRep f => TypeRep (M1 D d f) where
  typeRep (M1 src) = typeRep src

instance FieldRep f => TypeRep (M1 C c f) where
  typeRep (M1 src) = fieldRep src

--- FIELDS
class FieldRep f where
  fieldRep :: f a -> [(FieldName, String)]

instance (FieldRep f, FieldRep g) => FieldRep (f :*: g) where
  fieldRep (a :*: b) = fieldRep a <> fieldRep b

instance (Selector s, GetChannel a) => FieldRep (M1 S s (K1 s2 a)) where
  fieldRep m@(M1 (K1 src)) = [(FieldName $ pack (selName m), getChannel src)]

instance FieldRep U1 where
  fieldRep _ = []
