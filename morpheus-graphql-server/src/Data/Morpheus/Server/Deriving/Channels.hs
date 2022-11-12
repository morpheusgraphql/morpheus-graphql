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
  ( Decode,
    decodeArguments,
  )
import Data.Morpheus.Server.Deriving.Schema.Directive (toFieldRes)
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    DataType (..),
  )
import Data.Morpheus.Server.Deriving.Utils.DeriveGType
  ( DeriveValueOptions (..),
    DeriveWith,
    deriveValue,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (CatType (..), outputType)
import Data.Morpheus.Server.Types.GQLType (GQLType, deriveTypename, withDir, __typeData)
import Data.Morpheus.Server.Types.Types (Undefined)
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
import Relude hiding (Undefined)

newtype DerivedChannel e = DerivedChannel
  { _unpackChannel :: Channel e
  }

type ChannelRes (e :: Type) = Selection VALID -> ResolverState (DerivedChannel e)

type ChannelsConstraint e m (subs :: (Type -> Type) -> Type) =
  ExploreChannels (IsUndefined (subs (Resolver SUBSCRIPTION e m))) e (subs (Resolver SUBSCRIPTION e m))

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
    channelSelector =
      selectBySelection
        ( exploreChannels
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

class GetChannel e a | a -> e where
  getChannel :: a -> ChannelRes e

instance GetChannel e (SubscriptionField (Resolver SUBSCRIPTION e m a)) where
  getChannel x = const $ pure $ DerivedChannel $ channel x

instance
  Decode arg =>
  GetChannel e (arg -> SubscriptionField (Resolver SUBSCRIPTION e m a))
  where
  getChannel f sel@Selection {selectionArguments} =
    decodeArguments selectionArguments
      >>= (`getChannel` sel)
        . f

------------------------------------------------------

type family IsUndefined a :: Bool where
  IsUndefined (Undefined m) = 'True
  IsUndefined a = 'False

class ExploreChannels (t :: Bool) e a where
  exploreChannels :: f t -> a -> HashMap FieldName (ChannelRes e)

instance (GQLType a, Generic a, DeriveWith GQLType (GetChannel e) (ChannelRes e) (Rep a)) => ExploreChannels 'False e a where
  exploreChannels _ =
    HM.fromList
      . map (toFieldRes withDir (Proxy @a))
      . consFields
      . tyCons
      . deriveValue
        ( DeriveValueOptions
            { __valueApply = getChannel,
              __valueTypeName = deriveTypename (OutputType :: CatType OUT a),
              __valueGetType = __typeData . outputType
            } ::
            DeriveValueOptions OUT GQLType (GetChannel e) (ChannelRes e)
        )

instance ExploreChannels 'True e (Undefined m) where
  exploreChannels _ = pure HM.empty
