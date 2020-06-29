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
    Eventless,
    Resolver,
    SubscriptionField (..),
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Text
  ( pack,
  )
import GHC.Generics

--newtype Proxy e = Proxy (Selection VALID)

type ChannelCon e m a =
  ( TypeRep e (Rep (a (Resolver SUBSCRIPTION e m))),
    Generic (a (Resolver SUBSCRIPTION e m))
  )

class GetChannels (e :: *) a | a -> e where
  getChannels :: Selection VALID -> a -> Eventless (Channel e)

instance
  {-# OVERLAPPABLE #-}
  ChannelCon e m subs =>
  GetChannels e (subs (Resolver SUBSCRIPTION e m))
  where
  getChannels sel = selectBy sel . typeRep (Proxy @e) . from

selectBy ::
  Selection VALID ->
  [ ( FieldName,
      Selection VALID -> Eventless (Channel e)
    )
  ] ->
  Eventless (Channel e)
selectBy = undefined

class GetChannel e a | a -> e where
  getChannel :: a -> Selection VALID -> Eventless (Channel e)

instance GetChannel e (SubscriptionField (Resolver SUBSCRIPTION e m a)) where
  getChannel SubscriptionField {channel} = const (pure channel)

instance GetChannel e (arg -> SubscriptionField (Resolver SUBSCRIPTION e m a)) where
  getChannel = undefined --TODO:

------------------------------------------------------
class TypeRep e f where
  typeRep :: Proxy e -> f a -> [(FieldName, Selection VALID -> Eventless (Channel e))]

instance TypeRep e f => TypeRep e (M1 D d f) where
  typeRep c (M1 src) = typeRep c src

instance FieldRep e f => TypeRep e (M1 C c f) where
  typeRep c (M1 src) = fieldRep c src

--- FIELDS
class FieldRep e f where
  fieldRep :: Proxy e -> f a -> [(FieldName, Selection VALID -> Eventless (Channel e))]

instance (FieldRep e f, FieldRep e g) => FieldRep e (f :*: g) where
  fieldRep e (a :*: b) = fieldRep e a <> fieldRep e b

instance (Selector s, GetChannel e a) => FieldRep e (M1 S s (K1 s2 a)) where
  fieldRep _ m@(M1 (K1 src)) = [(FieldName $ pack (selName m), getChannel src)]

instance FieldRep e U1 where
  fieldRep _ _ = []
