{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Deriving.Channels
  ( GetChannels (..),
    ChannelCon,
  )
where

-- MORPHEUS

import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    SUBSCRIPTION,
    Selection,
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Channel,
    Resolver,
    SubscriptionField (..),
  )
import Data.Semigroup ((<>))
import Data.Text
  ( pack,
  )
import GHC.Generics

newtype Proxy e = Proxy (Selection VALID)

type ChannelCon e m a =
  ( TypeRep e (Rep (a (Resolver SUBSCRIPTION e m))),
    Generic (a (Resolver SUBSCRIPTION e m))
  )

class GetChannels (e :: *) a | a -> e where
  getChannels :: Selection VALID -> a -> [(FieldName, Channel e)]

instance
  {-# OVERLAPPABLE #-}
  ChannelCon e m subs =>
  GetChannels e (subs (Resolver SUBSCRIPTION e m))
  where
  getChannels sel = typeRep (Proxy sel :: Proxy e) . from

class GetChannel e a | a -> e where
  getChannel :: Selection VALID -> a -> Channel e

instance GetChannel e (SubscriptionField (Resolver SUBSCRIPTION e m a)) where
  getChannel _ SubscriptionField {channel} = channel

class TypeRep e f where
  typeRep :: Proxy e -> f a -> [(FieldName, Channel e)]

instance TypeRep e f => TypeRep e (M1 D d f) where
  typeRep c (M1 src) = typeRep c src

instance FieldRep e f => TypeRep e (M1 C c f) where
  typeRep c (M1 src) = fieldRep c src

--- FIELDS
class FieldRep e f where
  fieldRep :: Proxy e -> f a -> [(FieldName, Channel e)]

instance (FieldRep e f, FieldRep e g) => FieldRep e (f :*: g) where
  fieldRep e (a :*: b) = fieldRep e a <> fieldRep e b

instance (Selector s, GetChannel e a) => FieldRep e (M1 S s (K1 s2 a)) where
  fieldRep (Proxy sel) m@(M1 (K1 src)) = case selectBy name sel of
    Just sel' -> [(name, getChannel sel' src)]
    Nothing -> []
    where
      name = FieldName $ pack (selName m)

selectBy :: FieldName -> Selection VALID -> Maybe (Selection VALID)
selectBy = undefined

instance FieldRep e U1 where
  fieldRep _ _ = []
