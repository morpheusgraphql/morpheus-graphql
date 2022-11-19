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
import Data.Morpheus.Server.Deriving.Named.EncodeType (EncodeTypeConstraint, deriveResolver)
import Data.Morpheus.Server.Deriving.Utils.GTraversable (traverseTypes)
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
    withDir,
    withRes,
  )
import Data.Morpheus.Types.Internal.AST
  ( MUTATION,
    OperationType,
    QUERY,
    SUBSCRIPTION,
  )
import qualified GHC.Exts as HM
import Relude

type ROOT (o :: OperationType) e (m :: Type -> Type) a = EXPLORE GQLType GQLResolver (Resolver o e m) (a (Resolver o e m))

type DERIVE_RESOLVERS e m query mut sub =
  ( CHANNELS GQLType GQLValue e m sub,
    ROOT QUERY e m query,
    ROOT MUTATION e m mut,
    ROOT SUBSCRIPTION e m sub
  )

type DERIVE_NAMED_RESOLVERS e m query mut sub = (EncodeTypeConstraint (Resolver QUERY e m) query)

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
  (Monad m, DERIVE_NAMED_RESOLVERS e m query mut sub) =>
  NamedResolvers m e query mut sub ->
  RootResolverValue e m
deriveNamedResolvers NamedResolvers =
  NamedResolversValue $
    HM.fromList $
      map (\x -> (resolverName x, x)) $
        join $
          toList $
            traverseTypes deriveResolver (Proxy @(query (NamedResolverT (Resolver QUERY e m))))
