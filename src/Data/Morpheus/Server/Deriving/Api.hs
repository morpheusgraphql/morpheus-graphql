{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Server.Deriving.Api
  ( RootResolverConstraint,
    deriveSchema,
    deriveApi,
  )
where

import Data.Functor.Identity (Identity (..))
-- MORPHEUS

import Data.Morpheus.Core
  ( Api (..),
    mkApi,
  )
import Data.Morpheus.Server.Deriving.Channels (ChannelCon)
import Data.Morpheus.Server.Deriving.Encode
  ( EncodeCon,
    deriveModel,
  )
import Data.Morpheus.Server.Deriving.Introspect
  ( IntroCon,
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

type OperationConstraint operation event m a =
  ( EncodeCon operation event m (a (Resolver operation event m)),
    IntroCon (a (Resolver operation event m))
  )

type RootResolverConstraint m event query mutation subscription =
  ( Monad m,
    OperationConstraint QUERY event m query,
    OperationConstraint MUTATION event m mutation,
    OperationConstraint SUBSCRIPTION event m subscription,
    ChannelCon event m subscription
  )

deriveApi ::
  RootResolverConstraint m event query mut sub =>
  RootResolver m event query mut sub ->
  Api event m
deriveApi root = case deriveSchema (Identity root) of
  Success {result} -> mkApi result (deriveModel root)
  Failure {errors} -> FailApi errors
