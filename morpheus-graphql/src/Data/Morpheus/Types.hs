{-# LANGUAGE FlexibleContexts #-}
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
    Describe (..),
    VisitEnum (..),
    typeDirective,
    fieldDirective,
    enumDirective,

    -- * default GQL directives
    GQLDirective (..),
    Deprecated (..),
    dropNamespaceOptions,
    DropNamespace (..),
    Rename (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Morpheus.Server.Resolvers
import Data.Morpheus.Server.Types
  ( App,
    Arg (..),
    DecodeScalar (..),
    DecodeWrapper (..),
    Deprecated (..),
    Describe (..),
    DropNamespace (..),
    EncodeScalar (..),
    EncodeWrapper (..),
    GQLDirective (..),
    GQLRequest (..),
    GQLResponse (..),
    GQLType (..),
    GQLTypeOptions (..),
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
    defaultTypeOptions,
    dropNamespaceOptions,
    enumDirective,
    fieldDirective,
    render,
    subscribe,
    typeDirective,
    unsafeInternalContext,
  )
import Data.Morpheus.Types.Internal.AST (GQLError)
import Relude hiding (Undefined)

{-# DEPRECATED Res "use ResolverQ" #-}

type Res = Resolver QUERY

{-# DEPRECATED MutRes "use ResolverM" #-}

type MutRes = Resolver MUTATION

{-# DEPRECATED SubRes "use ResolverS" #-}

type SubRes = Resolver SUBSCRIPTION

{-# DEPRECATED IORes "use ResolverQ" #-}

type IORes e = Res e IO

{-# DEPRECATED IOMutRes "use ResolverM" #-}

type IOMutRes e = MutRes e IO

{-# DEPRECATED IOSubRes "use ResolverS" #-}

type IOSubRes e = SubRes e IO

{-# DEPRECATED ResolveQ "use ResolverQ" #-}

type ResolveQ e m a = ResolverQ e m a

{-# DEPRECATED ResolveM "use ResolverM" #-}

type ResolveM e m a = ResolverM e m a

{-# DEPRECATED ResolveS "use ResolverS" #-}

type ResolveS e m a = ResolverS e m a

constMutRes :: Monad m => [e] -> a -> args -> ResolverM e m a
constMutRes events v = const $ do
  publish events
  pure v

{-# DEPRECATED failRes "use \"fail\" from \"MonadFail\"" #-}
failRes ::
  ( Monad m,
    WithOperation o
  ) =>
  String ->
  Resolver o e m a
failRes = fail

liftEither :: (MonadTrans t, Monad (t m), MonadError GQLError (t m)) => Monad m => m (Either String a) -> t m a
liftEither x = lift x >>= either (throwError . fromString) pure
