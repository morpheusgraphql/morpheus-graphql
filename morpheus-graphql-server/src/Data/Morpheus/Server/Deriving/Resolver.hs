{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Resolver
  ( deriveResolvers,
    deriveNamedResolvers,
    DERIVE_RESOLVERS,
    DERIVE_NAMED_RESOLVERS,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( NamedResolver (..),
    Resolver,
    ResolverValue,
    RootResolverValue (..),
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Internal.Resolve.Explore
  ( EXPLORE,
    useObjectResolvers,
  )
import Data.Morpheus.Server.Deriving.Kinded.Channels
  ( CHANNELS,
    resolverChannels,
  )
import Data.Morpheus.Server.Deriving.Kinded.NamedResolver
  ( KindedNamedResolver (..),
  )
import Data.Morpheus.Server.Deriving.Kinded.NamedResolverFun (KindedNamedFunValue (..))
import Data.Morpheus.Server.Deriving.Utils.GTraversable (GFmap, Mappable (..), ScanConstraint, buildMap)
import Data.Morpheus.Server.Deriving.Utils.Kinded (KindedProxy (..))
import Data.Morpheus.Server.Deriving.Utils.Proxy
import Data.Morpheus.Server.Deriving.Utils.Use (UseNamedResolver (..))
import Data.Morpheus.Server.Resolvers
  ( NamedResolverT (..),
    NamedResolvers (..),
    RootResolver (..),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLResolver,
    GQLType (..),
    GQLValue,
    ignoreUndefined,
    kindedProxy,
    withDir,
    withRes,
  )
import Data.Morpheus.Types.Internal.AST
  ( MUTATION,
    OperationType,
    QUERY,
    SUBSCRIPTION,
  )
import Relude

class GQLNamedResolverFun (m :: Type -> Type) res where
  deriveNamedResFun :: res -> m (ResolverValue m)

class GQLNamedResolver (m :: Type -> Type) a where
  deriveNamedRes :: f a -> [NamedResolver m]

instance
  KindedNamedResolver GQLNamedResolver GQLNamedResolverFun GQLType GQLValue m (KIND a) a =>
  GQLNamedResolver m a
  where
  deriveNamedRes = kindedNamedResolver withNamed . kindedProxy

instance KindedNamedFunValue GQLNamedResolverFun GQLType GQLValue (KIND a) m a => GQLNamedResolverFun m a where
  deriveNamedResFun resolver = kindedNamedFunValue withNamed (ContextValue resolver :: ContextValue (KIND a) a)

withNamed :: UseNamedResolver GQLNamedResolver GQLNamedResolverFun GQLType GQLValue
withNamed =
  UseNamedResolver
    { namedDrv = withDir,
      useNamedFieldResolver = deriveNamedResFun,
      useDeriveNamedResolvers = deriveNamedRes
    }

deriveNamedResolver :: Mappable (KindedNamedResolver GQLNamedResolver GQLNamedResolverFun GQLType GQLValue m) [NamedResolver m] KindedProxy
deriveNamedResolver = Mappable (kindedNamedResolver withNamed)

type ROOT (o :: OperationType) e (m :: Type -> Type) a = EXPLORE GQLType GQLResolver (Resolver o e m) (a (Resolver o e m))

type DERIVE_RESOLVERS e m query mut sub =
  ( CHANNELS GQLType GQLValue e m sub,
    ROOT QUERY e m query,
    ROOT MUTATION e m mut,
    ROOT SUBSCRIPTION e m sub
  )

type DERIVE_NAMED_RESOLVERS e m query =
  ( GQLType (query (NamedResolverT (Resolver QUERY e m))),
    KindedNamedResolver
      GQLNamedResolver
      GQLNamedResolverFun
      GQLType
      GQLValue
      (Resolver QUERY e m)
      (KIND (query (NamedResolverT (Resolver QUERY e m))))
      (query (NamedResolverT (Resolver QUERY e m))),
    GFmap
      ( ScanConstraint
          ( KindedNamedResolver
              GQLNamedResolver
              GQLNamedResolverFun
              GQLType
              GQLValue
              (Resolver QUERY e m)
          )
      )
      (KIND (query (NamedResolverT (Resolver QUERY e m))))
      (query (NamedResolverT (Resolver QUERY e m)))
  )

deriveResolvers ::
  (Monad m, DERIVE_RESOLVERS e m query mut sub) =>
  RootResolver m e query mut sub ->
  GQLResult (RootResolverValue e m)
deriveResolvers RootResolver {..} =
  pure
    RootResolverValue
      { queryResolver = useObjectResolvers withRes queryResolver,
        mutationResolver = useObjectResolvers withRes mutationResolver,
        subscriptionResolver = useObjectResolvers withRes subscriptionResolver,
        channelMap =
          ignoreUndefined (Identity subscriptionResolver)
            $> resolverChannels withDir subscriptionResolver
      }

deriveNamedResolvers ::
  forall e m query mut sub.
  (Monad m, DERIVE_NAMED_RESOLVERS e m query) =>
  NamedResolvers m e query mut sub ->
  RootResolverValue e m
deriveNamedResolvers NamedResolvers =
  NamedResolversValue $
    buildMap
      resolverName
      deriveNamedResolver
      (Proxy @(query (NamedResolverT (Resolver QUERY e m))))
