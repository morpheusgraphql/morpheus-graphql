{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Channels
  ( getChannels,
    ChannelsConstraint,
  )
where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
  )
import Data.Morpheus.Server.Deriving.Decode
  ( DecodeConstraint,
    decodeArguments,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    DataType (..),
    FieldRep (..),
    TypeConstraint (..),
    TypeRep (..),
    toValue,
  )
import Data.Morpheus.Server.Types.GQLType (GQLType)
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
import GHC.Generics
import Prelude
  ( (.),
    const,
    lookup,
    map,
  )

type ChannelsConstraint e m (subs :: (* -> *) -> *) =
  ExploreConstraint e (subs (Resolver SUBSCRIPTION e m))

getChannels ::
  ChannelsConstraint e m subs =>
  subs (Resolver SUBSCRIPTION e m) ->
  Selection VALID ->
  ResolverState (Channel e)
getChannels value sel = selectBy sel (exploreChannels value)

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
  DecodeConstraint arg =>
  GetChannel e (arg -> SubscriptionField (Resolver SUBSCRIPTION e m a))
  where
  getChannel f sel@Selection {selectionArguments} =
    decodeArguments selectionArguments >>= (`getChannel` sel) . f

------------------------------------------------------

type ChannelRes e = Selection VALID -> ResolverState (Channel e)

type ExploreConstraint e a =
  ( GQLType a,
    Generic a,
    TypeRep (GetChannel e) (Selection VALID -> ResolverState (Channel e)) (Rep a)
  )

exploreChannels :: forall e a. ExploreConstraint e a => a -> [(FieldName, ChannelRes e)]
exploreChannels =
  convertNode
    . toValue
      ( TypeConstraint (getChannel . runIdentity) ::
          TypeConstraint (GetChannel e) (Selection VALID -> ResolverState (Channel e)) Identity
      )

convertNode :: DataType (ChannelRes e) -> [(FieldName, ChannelRes e)]
convertNode DataType {tyCons = ConsRep {consFields}} = map toChannels consFields
  where
    toChannels FieldRep {fieldSelector, fieldValue} = (fieldSelector, fieldValue)
