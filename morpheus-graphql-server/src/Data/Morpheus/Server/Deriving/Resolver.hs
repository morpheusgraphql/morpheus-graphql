{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
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
import Data.Morpheus.Server.Resolvers
  ( RootResolver (..),
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
import Relude

type ROOT (o :: OperationType) e (m :: Type -> Type) a = EXPLORE GQLType GQLResolver (Resolver o e m) (a (Resolver o e m))

type EncodeConstraints e m query mut sub =
  ( CHANNELS GQLType GQLValue e m sub,
    ROOT QUERY e m query,
    ROOT MUTATION e m mut,
    ROOT SUBSCRIPTION e m sub
  )

deriveResolvers ::
  (Monad m, EncodeConstraints e m query mut sub) =>
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
