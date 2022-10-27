{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Data.Morpheus.Types
  ( GQLType (..),
    EncodeScalar (..),
    EncodeWrapper (..),
    DecodeScalar (..),
    DecodeWrapper (..),
    GQLRequest (..),
    GQLResponse (..),
    ID (..),
    ScalarValue (..),
    RootResolver (..),
    constRes,
    Undefined,
    Resolver,
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    lift,
    liftEither,
    WithOperation,
    publish,
    subscribe,
    ResolverContext (..),
    ResolverO,
    ComposedResolver,
    ResolverQ,
    ResolverM,
    ResolverS,
    SubscriptionField,
    App,
    RenderGQL,
    render,
    TypeGuard (..),
    Arg (..),
    NamedResolvers (..),

    -- * GQLType naming configuration
    defaultRootResolver,

    -- * GQL directives API
    Prefixes (..),
    VisitType (..),
    VisitField (..),
    Describe (..),
    VisitEnum (..),
    typeDirective,
    fieldDirective,
    enumDirective,

    -- * default GQL directives
    GQLDirective (..),
    Deprecated (..),
    DropNamespace (..),
    Rename (..),
    DefaultValue (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Morpheus.Server.Resolvers
import Data.Morpheus.Server.Types
  ( App,
    Arg (..),
    DecodeScalar (..),
    DecodeWrapper (..),
    DefaultValue (..),
    Deprecated (..),
    Describe (..),
    DropNamespace (..),
    EncodeScalar (..),
    EncodeWrapper (..),
    GQLDirective (..),
    GQLRequest (..),
    GQLResponse (..),
    GQLType (..),
    ID (..),
    MUTATION,
    Prefixes (..),
    QUERY,
    Rename (..),
    RenderGQL,
    Resolver,
    ResolverContext (..),
    SUBSCRIPTION,
    ScalarValue (..),
    SubscriptionField,
    TypeGuard (..),
    Undefined,
    VisitEnum (..),
    VisitField (..),
    VisitType (..),
    WithOperation,
    enumDirective,
    fieldDirective,
    render,
    subscribe,
    typeDirective,
  )
import Data.Morpheus.Types.Internal.AST (GQLError)
import Relude hiding (Undefined)

liftEither :: (MonadTrans t, Monad (t m), MonadError GQLError (t m)) => Monad m => m (Either String a) -> t m a
liftEither x = lift x >>= either (throwError . fromString) pure
