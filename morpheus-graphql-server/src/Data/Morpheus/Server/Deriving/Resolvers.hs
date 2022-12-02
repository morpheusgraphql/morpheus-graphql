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

module Data.Morpheus.Server.Deriving.Resolvers
  ( deriveResolvers,
    deriveNamedResolvers,
    DERIVE_RESOLVERS,
    DERIVE_NAMED_RESOLVERS,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( MonadResolver (MonadMutation, MonadQuery, MonadSubscription),
    NamedResolver (..),
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
import Data.Morpheus.Server.Deriving.Utils.GScan
  ( ScanRef,
    Scanner (..),
    scan,
  )
import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( ContextValue (..),
  )
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
  ( QUERY,
  )
import Relude

class GQLNamedResolverFun (m :: Type -> Type) a where
  deriveNamedResFun :: a -> m (ResolverValue m)

class GQLType a => GQLNamedResolver (m :: Type -> Type) a where
  deriveNamedRes :: f a -> [NamedResolver m]
  deriveNamedRefs :: f a -> [ScanRef (GQLNamedResolver m)]

instance
  (GQLType a, KindedNamedResolver GQLNamedResolver GQLNamedResolverFun GQLType GQLValue m (KIND a) a) =>
  GQLNamedResolver m a
  where
  deriveNamedRes = kindedNamedResolver withNamed . kindedProxy
  deriveNamedRefs = kindedNamedRefs withNamed . kindedProxy

instance KindedNamedFunValue GQLNamedResolverFun GQLType GQLValue (KIND a) m a => GQLNamedResolverFun m a where
  deriveNamedResFun resolver = kindedNamedFunValue withNamed (ContextValue resolver :: ContextValue (KIND a) a)

withNamed :: UseNamedResolver GQLNamedResolver GQLNamedResolverFun GQLType GQLValue
withNamed =
  UseNamedResolver
    { namedDrv = withDir,
      useNamedFieldResolver = deriveNamedResFun,
      useDeriveNamedResolvers = deriveNamedRes,
      useDeriveNamedRefs = deriveNamedRefs
    }

deriveNamedResolver :: Scanner (GQLNamedResolver m) (NamedResolver m)
deriveNamedResolver = Scanner {scannerFun = deriveNamedRes, scannerRefs = deriveNamedRefs}

type ROOT (m :: Type -> Type) a = EXPLORE GQLType GQLResolver m (a m)

type DERIVE_RESOLVERS m query mut sub =
  ( CHANNELS GQLType GQLValue sub (MonadSubscription m),
    ROOT (MonadQuery m) query,
    ROOT (MonadMutation m) mut,
    ROOT (MonadSubscription m) sub
  )

type DERIVE_NAMED_RESOLVERS m query =
  ( GQLType (query (NamedResolverT m)),
    KindedNamedResolver
      GQLNamedResolver
      GQLNamedResolverFun
      GQLType
      GQLValue
      m
      (KIND (query (NamedResolverT m)))
      (query (NamedResolverT m))
  )

deriveResolvers ::
  (Monad m, DERIVE_RESOLVERS (Resolver QUERY e m) query mut sub) =>
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
  (Monad m, DERIVE_NAMED_RESOLVERS (Resolver QUERY e m) query) =>
  NamedResolvers m e query mut sub ->
  RootResolverValue e m
deriveNamedResolvers NamedResolvers =
  NamedResolversValue $
    scan
      resolverName
      deriveNamedResolver
      (deriveNamedRefs (Proxy @(query (NamedResolverT (Resolver QUERY e m)))))
