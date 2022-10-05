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
    Undefined,
    Resolver,
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    lift,
    WithOperation,
    subscribe,
    unsafeInternalContext,
    ResolverContext (..),
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
    dropNamespaceOptions,
    SCALAR,
    DerivingKind (..),
    TYPE,
    CUSTOM,
    WRAPPER,
    RootResolver (..),
    defaultRootResolver,
    Rename (..),
  )
where

import Data.Morpheus.App
  ( App,
  )
import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
    ResolverContext (..),
    SubscriptionField,
    WithOperation,
    subscribe,
    unsafeInternalContext,
  )
import Data.Morpheus.Core
  ( RenderGQL,
    render,
  )
import Data.Morpheus.Server.Deriving.Encode ()
import Data.Morpheus.Server.Resolvers
  ( RootResolver (..),
    defaultRootResolver,
  )
import Data.Morpheus.Server.Types.DirectiveDefinitions
  ( Deprecated (..),
    Describe (..),
    Prefixes (..),
    Rename (..),
  )
import Data.Morpheus.Server.Types.Directives (GQLDirective (..))
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    enumDirective,
    enumDirective',
    fieldDirective,
    fieldDirective',
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
  ( MUTATION,
    QUERY,
    SUBSCRIPTION,
    ScalarValue (..),
  )
import Relude hiding (Undefined)
