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
    Undefined,
    Resolver,
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    lift,
    WithOperation,
    subscribe,
    ResolverContext (..),
    SubscriptionField,
    App,
    RenderGQL,
    render,
    TypeGuard (..),
    Arg (..),
    GQLError,

    -- * GQL directives API
    Prefixes (..),
    VisitType (..),
    VisitField (..),
    VisitEnum (..),
    typeDirective,
    fieldDirective,
    enumDirective,
    fieldDirective',
    enumDirective',

    -- * default GQL directives
    GQLDirective (..),
    Deprecated (..),
    Describe (..),
    SCALAR,
    DerivingKind,
    TYPE,
    CUSTOM,
    WRAPPER,
    RootResolver (..),
    defaultRootResolver,
    Rename (..),
    InputTypeNamespace (..),
    DropNamespace (..),
    DefaultValue (..),
    Value (..),
    DirectiveLocation (..),
    MonadResolver (..),
  )
where

import Data.Morpheus.App
  ( App,
  )
import Data.Morpheus.App.Internal.Resolving
  ( MonadResolver (..),
    Resolver,
    ResolverContext (..),
    SubscriptionField,
    WithOperation,
    subscribe,
  )
import Data.Morpheus.Core
  ( RenderGQL,
    render,
  )
import Data.Morpheus.Server.Resolvers
  ( RootResolver (..),
    defaultRootResolver,
  )
import Data.Morpheus.Server.Types.DirectiveDefinitions
  ( DefaultValue (..),
    Deprecated (..),
    Describe (..),
    DropNamespace (..),
    Prefixes (..),
    Rename (..),
  )
import Data.Morpheus.Server.Types.Directives
  ( GQLDirective (..),
    enumDirective,
    enumDirective',
    fieldDirective,
    fieldDirective',
    typeDirective,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    InputTypeNamespace (..),
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
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
  ( DirectiveLocation (..),
    GQLError,
    MUTATION,
    QUERY,
    SUBSCRIPTION,
    ScalarValue (..),
    Value (..),
  )
import Relude hiding (Undefined)
