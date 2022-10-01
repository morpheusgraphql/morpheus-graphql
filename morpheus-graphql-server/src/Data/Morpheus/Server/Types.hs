{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Data.Morpheus.Server.Types
  ( GQLType
      ( KIND,
        description,
        getDescriptions,
        typeOptions,
        getDirectives,
        defaultValues,
        directives
      ),
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
    constMutRes,
    Undefined,
    Resolver,
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    lift,
    liftEither,
    failRes,
    WithOperation,
    publish,
    subscribe,
    unsafeInternalContext,
    ResolverContext (..),
    ResolverO,
    ComposedResolver,
    ResolverQ,
    ResolverM,
    ResolverS,
    ResolveQ,
    ResolveM,
    ResolveS,
    Res,
    MutRes,
    SubRes,
    IORes,
    IOMutRes,
    IOSubRes,
    SubscriptionField,
    App,
    RenderGQL,
    render,
    TypeGuard (..),
    Arg (..),

    -- * GQLType naming configuration
    GQLTypeOptions,
    defaultTypeOptions,
    fieldLabelModifier,
    constructorTagModifier,
    typeNameModifier,
    defaultRootResolver,

    -- * GQL directives API
    Prefixes (..),
    VisitType (..),
    VisitField (..),
    VisitEnum (..),
    typeDirective,
    fieldDirective,
    enumDirective,

    -- * default GQL directives
    GQLDirective (..),
    Deprecated (..),
    dropNamespaceOptions,
    SCALAR,
    DerivingKind (..),
    TYPE,
    CUSTOM,
    WRAPPER,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Morpheus.App
  ( App,
  )
import Data.Morpheus.App.Internal.Resolving
  ( PushEvents (..),
    Resolver,
    ResolverContext (..),
    SubscriptionField,
    WithOperation,
    pushEvents,
    subscribe,
    unsafeInternalContext,
  )
import Data.Morpheus.Core
  ( RenderGQL,
    render,
  )
-- FIXME: TO ENABLE DECODE INSTANCE ON DIRECTIVES
import Data.Morpheus.Server.Deriving.Decode ()
import Data.Morpheus.Server.NamedResolvers
  ( NamedResolverT (..),
    ResolveNamed (..),
  )
import Data.Morpheus.Server.Types.DirectiveDefinitions
import Data.Morpheus.Server.Types.Directives
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    enumDirective,
    fieldDirective,
    typeDirective,
  )
import Data.Morpheus.Server.Types.Internal
  ( GQLTypeOptions (..),
    defaultTypeOptions,
    dropNamespaceOptions,
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind (..),
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.Types
  ( Arg (..),
    TypeGuard (..),
    Undefined (..),
  )
import Data.Morpheus.Server.Types.Visitors
  ( VisitEnum (..),
    VisitField (..),
    VisitType (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
  )
import Data.Morpheus.Types.GQLWrapper
  ( DecodeWrapper (..),
    EncodeWrapper (..),
  )
import Data.Morpheus.Types.ID (ID (..))
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    MUTATION,
    QUERY,
    SUBSCRIPTION,
    ScalarValue (..),
  )
import Relude hiding (Undefined)

class FlexibleResolver (f :: Type -> Type) (a :: k) where
  type Flexible (m :: Type -> Type) a :: Type
  type Composed (m :: Type -> Type) f a :: Type

instance FlexibleResolver f (a :: Type) where
  type Flexible m a = m a
  type Composed m f a = m (f a)

instance FlexibleResolver f (a :: (Type -> Type) -> Type) where
  type Flexible m a = m (a m)
  type Composed m f a = m (f (a m))

type ResolverO o e m a = Flexible (Resolver o e m) a

type ComposedResolver o e m f a = Composed (Resolver o e m) f a

publish :: Monad m => [e] -> Resolver MUTATION e m ()
publish = pushEvents

constRes :: (WithOperation o, Monad m) => b -> a -> Resolver o e m b
constRes = const . pure
