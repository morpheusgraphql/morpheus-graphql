{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Server.Deriving.App
  ( RootResolverConstraint,
    deriveSchema,
    deriveApp,
  )
where

import Data.Functor.Identity (Identity (..))
-- MORPHEUS

import Data.Morpheus.Core
  ( App (..),
    mkApp,
  )
import Data.Morpheus.Server.Deriving.Channels (ChannelCon)
import Data.Morpheus.Server.Deriving.Encode
  ( EncodeConstraints,
    deriveModel,
  )
import Data.Morpheus.Server.Deriving.Introspect
  ( SchemaConstraints,
    deriveSchema,
  )
import Data.Morpheus.Types
  ( RootResolver (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( MUTATION,
    QUERY,
    SUBSCRIPTION,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Resolver,
    Result (..),
  )

type RootResolverConstraint m event query mutation subscription =
  ( EncodeConstraints event m query mutation subscription,
    SchemaConstraints event m query mutation subscription
  )

deriveApp ::
  RootResolverConstraint m event query mut sub =>
  RootResolver m event query mut sub ->
  App event m
deriveApp root = case deriveSchema (Identity root) of
  Success {result} -> mkApp result (deriveModel root)
  Failure {errors} -> FailApp errors
