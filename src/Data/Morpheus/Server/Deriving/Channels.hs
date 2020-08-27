{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Channels
  ( getChannels,
    ChannelCon,
    GetChannel (..),
    ExploreChannels (..),
  )
where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
  )
import Data.Morpheus.Server.Deriving.Decode
  ( DecodeType,
    decodeArguments,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    InternalError,
    SUBSCRIPTION,
    Selection (..),
    SelectionContent (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Channel,
    Resolver,
    ResolverState,
    SubscriptionField (..),
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Text
  ( pack,
  )
import GHC.Generics
import Prelude
  ( ($),
    (.),
    const,
    lookup,
  )

type ChannelCon e m a = ExploreChannels (a (Resolver SUBSCRIPTION e m)) e

getChannels ::
  forall e m subs.
  ChannelCon e m subs =>
  subs (Resolver SUBSCRIPTION e m) ->
  Selection VALID ->
  ResolverState (Channel e)
getChannels value sel =
  selectBy sel $
    exploreChannels (Proxy @e) value

selectBy ::
  Failure InternalError m =>
  Selection VALID ->
  [ ( FieldName,
      Selection VALID -> m (Channel e)
    )
  ] ->
  m (Channel e)
selectBy Selection {selectionContent = SelectionSet selSet} ch =
  case elems selSet of
    [sel@Selection {selectionName}] -> case lookup selectionName ch of
      Nothing -> failure ("invalid subscription: no channel is selected." :: InternalError)
      Just f -> f sel
    _ -> failure ("invalid subscription: there can be only one top level selection" :: InternalError)
selectBy _ _ = failure ("invalid subscription: expected selectionSet" :: InternalError)

class GetChannel e a | a -> e where
  getChannel :: a -> Selection VALID -> ResolverState (Channel e)

instance GetChannel e (SubscriptionField (Resolver SUBSCRIPTION e m a)) where
  getChannel SubscriptionField {channel} = const (pure channel)

instance
  (Generic arg, DecodeType arg) =>
  GetChannel e (arg -> SubscriptionField (Resolver SUBSCRIPTION e m a))
  where
  getChannel f sel@Selection {selectionArguments} =
    decodeArguments selectionArguments >>= (`getChannel` sel) . f

------------------------------------------------------
class ExploreChannels a e where
  exploreChannels :: Proxy e -> a -> [(FieldName, Selection VALID -> ResolverState (Channel e))]

instance
  ( TypeRep e (Rep (subs (Resolver SUBSCRIPTION e m))),
    Generic (subs (Resolver SUBSCRIPTION e m))
  ) =>
  ExploreChannels (subs (Resolver SUBSCRIPTION e m)) e
  where
  exploreChannels _ = typeRep (Proxy @e) . from

------------------------------------------------------
class TypeRep e f where
  typeRep :: Proxy e -> f a -> [(FieldName, Selection VALID -> ResolverState (Channel e))]

instance TypeRep e f => TypeRep e (M1 D d f) where
  typeRep c (M1 src) = typeRep c src

instance FieldRep e f => TypeRep e (M1 C c f) where
  typeRep c (M1 src) = fieldRep c src

--- FIELDS
class FieldRep e f where
  fieldRep :: Proxy e -> f a -> [(FieldName, Selection VALID -> ResolverState (Channel e))]

instance (FieldRep e f, FieldRep e g) => FieldRep e (f :*: g) where
  fieldRep e (a :*: b) = fieldRep e a <> fieldRep e b

instance (Selector s, GetChannel e a) => FieldRep e (M1 S s (K1 s2 a)) where
  fieldRep _ m@(M1 (K1 src)) = [(FieldName $ pack (selName m), getChannel src)]

instance FieldRep e U1 where
  fieldRep _ _ = []
