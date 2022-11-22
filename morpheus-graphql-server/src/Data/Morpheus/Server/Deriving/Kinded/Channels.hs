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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Kinded.Channels
  ( resolverChannels,
    CHANNELS,
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
import Data.Morpheus.Server.Deriving.Internal.Decode.Utils (useDecodeArguments)
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive (UseDeriving (..), toFieldRes)
import Data.Morpheus.Server.Deriving.Utils.GRep
  ( GRep,
    RepContext (..),
    deriveValue,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (outputType)
import Data.Morpheus.Server.Deriving.Utils.Types
  ( ConsRep (..),
    DataType (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Use (UseGQLType (useTypeData))
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

type CHANNELS gql val e m (subs :: (Type -> Type) -> Type) =
  ExploreChannels gql val (IsUndefined (subs (Resolver SUBSCRIPTION e m))) e (subs (Resolver SUBSCRIPTION e m))

resolverChannels ::
  forall e m subs gql val.
  CHANNELS gql val e m subs =>
  UseDeriving gql val ->
  subs (Resolver SUBSCRIPTION e m) ->
  Selection VALID ->
  ResolverState (Channel e)
resolverChannels drv value = fmap _unpackChannel . channelSelector
  where
    channelSelector ::
      Selection VALID ->
      ResolverState (DerivedChannel e)
    channelSelector =
      selectBySelection
        ( exploreChannels
            drv
            (Proxy @(IsUndefined (subs (Resolver SUBSCRIPTION e m))))
            value
        )

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

class GetChannel val e a | a -> e where
  getChannel :: UseDeriving gql val -> a -> ChannelRes e

instance GetChannel val e (SubscriptionField (Resolver SUBSCRIPTION e m a)) where
  getChannel _ x = const $ pure $ DerivedChannel $ channel x

instance (val arg) => GetChannel val e (arg -> SubscriptionField (Resolver SUBSCRIPTION e m a)) where
  getChannel drv f sel@Selection {selectionArguments} =
    useDecodeArguments drv selectionArguments
      >>= flip (getChannel drv) sel . f

------------------------------------------------------

type family IsUndefined a :: Bool where
  IsUndefined (Undefined m) = TRUE
  IsUndefined a = FALSE

class ExploreChannels gql val (t :: Bool) e a where
  exploreChannels :: UseDeriving gql val -> f t -> a -> HashMap FieldName (ChannelRes e)

instance (gql a, Generic a, GRep gql (GetChannel val e) (ChannelRes e) (Rep a)) => ExploreChannels gql val FALSE e a where
  exploreChannels drv _ =
    HM.fromList
      . map (toFieldRes drv (Proxy @a))
      . consFields
      . tyCons
      . deriveValue
        ( RepContext
            { optApply = getChannel drv . runIdentity,
              optTypeData = useTypeData (dirGQL drv) . outputType
            } ::
            RepContext gql (GetChannel val e) Identity (ChannelRes e)
        )

instance ExploreChannels drv val TRUE e (Undefined m) where
  exploreChannels _ _ = pure HM.empty
