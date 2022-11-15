{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Resolver
  ( deriveResolvers,
    EncodeConstraints,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
    ResolverValue (..),
    RootResolverValue (..),
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Channels
  ( ChannelsConstraint,
    channelResolver,
  )
import Data.Morpheus.Server.Deriving.Internal.Resolve.Explore (EXPLORE, useObjectResolvers)
import Data.Morpheus.Server.Deriving.Kinded.Resolver (KindedResolver (kindedResolver))
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( ContextValue (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving,
    UseResolver (..),
  )
import Data.Morpheus.Server.Resolvers
  ( RootResolver (..),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    GQLValue,
    KIND,
    withDir,
    __isEmptyType,
  )
import Data.Morpheus.Types.Internal.AST
  ( MUTATION,
    OperationType,
    QUERY,
    SUBSCRIPTION,
  )
import Relude

ctx :: (UseResolver GQLResolver, UseDeriving GQLType GQLValue)
ctx = (UseResolver {useEncodeResolver = deriveResolver}, withDir)

class GQLResolver (m :: Type -> Type) resolver where
  deriveResolver :: resolver -> m (ResolverValue m)

instance (KindedResolver GQLType GQLResolver (KIND a) m a) => GQLResolver m a where
  deriveResolver resolver = kindedResolver ctx (ContextValue resolver :: ContextValue (KIND a) a)

type ROOT (o :: OperationType) e (m :: Type -> Type) a = EXPLORE GQLType GQLResolver (Resolver o e m) (a (Resolver o e m))

type EncodeConstraints e m query mut sub =
  ( ChannelsConstraint e m sub,
    ROOT QUERY e m query,
    ROOT MUTATION e m mut,
    ROOT SUBSCRIPTION e m sub
  )

deriveResolvers ::
  forall e m query mut sub.
  (Monad m, EncodeConstraints e m query mut sub) =>
  RootResolver m e query mut sub ->
  GQLResult (RootResolverValue e m)
deriveResolvers RootResolver {..} =
  pure
    RootResolverValue
      { queryResolver = useObjectResolvers ctx queryResolver,
        mutationResolver = useObjectResolvers ctx mutationResolver,
        subscriptionResolver = useObjectResolvers ctx subscriptionResolver,
        channelMap
      }
  where
    channelMap
      | __isEmptyType (Proxy :: Proxy (sub (Resolver SUBSCRIPTION e m))) = Nothing
      | otherwise = Just (channelResolver subscriptionResolver)
