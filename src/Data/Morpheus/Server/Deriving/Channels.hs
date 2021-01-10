{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Channels
  ( channelResolver,
    ChannelsConstraint,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( Channel,
    Resolver,
    ResolverState,
    SubscriptionField (..),
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
    selectBy,
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
    OUT,
    SUBSCRIPTION,
    Selection (..),
    SelectionContent (..),
    VALID,
  )
import GHC.Generics
import Relude

newtype DerivedChannel e = DerivedChannel
  { _unpackChannel :: Channel e
  }

type ChannelRes (e :: *) = Selection VALID -> ResolverState (DerivedChannel e)

type ChannelsConstraint e m (subs :: (* -> *) -> *) =
  ExploreConstraint e (subs (Resolver SUBSCRIPTION e m))

channelResolver ::
  forall e m subs.
  ChannelsConstraint e m subs =>
  subs (Resolver SUBSCRIPTION e m) ->
  Selection VALID ->
  ResolverState (Channel e)
channelResolver value = fmap _unpackChannel . channelSelector
  where
    channelSelector ::
      Selection VALID ->
      ResolverState (DerivedChannel e)
    channelSelector = selectBySelection (exploreChannels value)

selectBySelection ::
  [(FieldName, ChannelRes e)] ->
  Selection VALID ->
  ResolverState (DerivedChannel e)
selectBySelection channels = withSubscriptionSelection >=> selectSubscription channels

selectSubscription ::
  [(FieldName, ChannelRes e)] ->
  Selection VALID ->
  ResolverState (DerivedChannel e)
selectSubscription channels sel@Selection {selectionName} =
  selectBy
    onFail
    selectionName
    channels
    >>= onSucc
  where
    onFail = "invalid subscription: no channel is selected." :: InternalError
    onSucc (_, f) = f sel

withSubscriptionSelection :: Selection VALID -> ResolverState (Selection VALID)
withSubscriptionSelection Selection {selectionContent = SelectionSet selSet} =
  case elems selSet of
    [sel] -> pure sel
    _ -> failure ("invalid subscription: there can be only one top level selection" :: InternalError)
withSubscriptionSelection _ = failure ("invalid subscription: expected selectionSet" :: InternalError)

class GetChannel e a | a -> e where
  getChannel :: a -> ChannelRes e

instance GetChannel e (SubscriptionField (Resolver SUBSCRIPTION e m a)) where
  getChannel = const . pure . DerivedChannel . channel

instance
  DecodeConstraint arg =>
  GetChannel e (arg -> SubscriptionField (Resolver SUBSCRIPTION e m a))
  where
  getChannel f sel@Selection {selectionArguments} =
    decodeArguments selectionArguments >>= (`getChannel` sel)
      . f

------------------------------------------------------

type ExploreConstraint e a =
  ( GQLType a,
    Generic a,
    TypeRep (GetChannel e) (ChannelRes e) (Rep a)
  )

exploreChannels :: forall e a. ExploreConstraint e a => a -> [(FieldName, ChannelRes e)]
exploreChannels =
  convertNode
    . toValue
      ( TypeConstraint (getChannel . runIdentity) :: TypeConstraint (GetChannel e) (ChannelRes e) Identity
      )
      (Proxy @OUT)

convertNode :: DataType (ChannelRes e) -> [(FieldName, ChannelRes e)]
convertNode DataType {tyCons = ConsRep {consFields}} = map toChannels consFields
  where
    toChannels FieldRep {fieldSelector, fieldValue} = (fieldSelector, fieldValue)
