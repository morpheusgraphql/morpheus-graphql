{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Kinded.Channels
  ( resolverChannels,
    CHANNELS,
  )
where

import Control.Monad.Except (throwError)
import Data.Morpheus.App.Internal.Resolving
  ( Channel,
    MonadResolver (..),
    ResolverState,
    SubscriptionField (..),
  )
import Data.Morpheus.Generic
  ( GRep,
    GRepField,
    GRepFun (..),
    GRepValue (..),
    deriveValue,
  )
import Data.Morpheus.Internal.Utils
  ( selectBy,
  )
import Data.Morpheus.Server.Deriving.Internal.Directive (UseDeriving (..), toFieldRes)
import Data.Morpheus.Server.Deriving.Utils.Kinded (outputType)
import Data.Morpheus.Server.Deriving.Utils.Use (UseGQLType (..), useDecodeArguments)
import Data.Morpheus.Server.Types.Types (Undefined)
import Data.Morpheus.Types.Internal.AST
  ( FALSE,
    FieldName,
    SUBSCRIPTION,
    Selection (..),
    SelectionContent (..),
    TRUE,
    VALID,
    internal,
  )
import GHC.Generics (Rep)
import Relude hiding (Undefined)

newtype DerivedChannel e = DerivedChannel
  { _unpackChannel :: Channel e
  }

type ChannelRes (e :: Type) = Selection VALID -> ResolverState (DerivedChannel e)

type CHANNELS gql val (subs :: (Type -> Type) -> Type) m =
  ( MonadResolver m,
    MonadOperation m ~ SUBSCRIPTION,
    ExploreChannels (UseDeriving gql val) (IsUndefined (subs m)) (MonadEvent m) (subs m)
  )

resolverChannels ::
  forall m subs gql val.
  (CHANNELS gql val subs m) =>
  UseDeriving gql val ->
  subs m ->
  Selection VALID ->
  ResolverState (Channel (MonadEvent m))
resolverChannels drv value = fmap _unpackChannel . channelSelector
  where
    channelSelector :: Selection VALID -> ResolverState (DerivedChannel (MonadEvent m))
    channelSelector = selectBySelection (exploreChannels drv (Proxy @(IsUndefined (subs m))) value)

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

class GetChannel val e a where
  getChannel :: UseDeriving gql val -> a -> ChannelRes e

instance (MonadResolver m, MonadOperation m ~ SUBSCRIPTION, MonadEvent m ~ e) => GetChannel val e (SubscriptionField (m a)) where
  getChannel _ x = const $ pure $ DerivedChannel $ channel x

instance (MonadResolver m, MonadOperation m ~ SUBSCRIPTION, MonadEvent m ~ e, val arg) => GetChannel val e (arg -> SubscriptionField (m a)) where
  getChannel drv f sel@Selection {selectionArguments} =
    useDecodeArguments drv selectionArguments
      >>= flip (getChannel drv) sel
      . f

------------------------------------------------------

type family IsUndefined a :: Bool where
  IsUndefined (Undefined m) = TRUE
  IsUndefined a = FALSE

class ExploreChannels ctx (t :: Bool) e a where
  exploreChannels :: (UseDeriving gql val ~ ctx) => ctx -> f t -> a -> HashMap FieldName (ChannelRes e)

instance (UseDeriving gql val ~ ctx, gql a, Generic a, GRep gql (GetChannel val e) (ChannelRes e) (Rep a)) => ExploreChannels ctx FALSE e a where
  exploreChannels ctx _ =
    fromList
      . map (toFieldRes ctx (Proxy @a))
      . toFields
      . deriveValue
        ( GRepFun
            { grepFun = getChannel ctx . runIdentity,
              grepTypename = useTypename ctx . outputType,
              grepWrappers = useWrappers ctx . outputType
            } ::
            GRepFun gql (GetChannel val e) Identity (ChannelRes e)
        )

toFields :: GRepValue (ChannelRes e) -> [GRepField (ChannelRes e)]
toFields GRepValueObject {..} = objectFields
toFields _ = []

instance ExploreChannels ctx TRUE e (Undefined m) where
  exploreChannels _ _ = pure mempty
