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

import Control.Monad.Except (throwError)
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving
  ( Channel,
    Resolver,
    ResolverState,
    SubscriptionField (..),
  )
import Data.Morpheus.Internal.Utils
  ( selectBy,
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
  ( FieldName,
    OUT,
    SUBSCRIPTION,
    Selection (..),
    SelectionContent (..),
    VALID,
    internal,
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
  HashMap FieldName (ChannelRes e) ->
  Selection VALID ->
  ResolverState (DerivedChannel e)
selectBySelection channels = withSubscriptionSelection >=> selectSubscription channels

selectSubscription ::
  HashMap FieldName (ChannelRes e) ->
  Selection VALID ->
  ResolverState (DerivedChannel e)
selectSubscription channels sel@Selection {selectionName} =
  selectBy
    (internal "invalid subscription: no channel is selected.")
    selectionName
    channels
    >>= (sel &)

withSubscriptionSelection :: Selection VALID -> ResolverState (Selection VALID)
withSubscriptionSelection Selection {selectionContent = SelectionSet selSet} =
  case toList selSet of
    [sel] -> pure sel
    _ -> throwError (internal "invalid subscription: there can be only one top level selection")
withSubscriptionSelection _ = throwError (internal "invalid subscription: expected selectionSet")

class GetChannel e a | a -> e where
  getChannel :: a -> ChannelRes e

instance GetChannel e (SubscriptionField (Resolver SUBSCRIPTION e m a)) where
  getChannel x = const $ pure $ DerivedChannel $ channel x

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

exploreChannels :: forall e a. ExploreConstraint e a => a -> HashMap FieldName (ChannelRes e)
exploreChannels =
  HM.fromList
    . convertNode
    . toValue
      ( TypeConstraint (getChannel . runIdentity) :: TypeConstraint (GetChannel e) (ChannelRes e) Identity
      )
      (Proxy @OUT)

convertNode :: DataType (ChannelRes e) -> [(FieldName, ChannelRes e)]
convertNode DataType {tyCons = ConsRep {consFields}} = map toChannels consFields
  where
    toChannels FieldRep {fieldSelector, fieldValue} = (fieldSelector, fieldValue)
