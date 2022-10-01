{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Data.Morpheus.Types
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
    NamedResolvers (..),

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
  )
where

import Data.Morpheus.Server.Types