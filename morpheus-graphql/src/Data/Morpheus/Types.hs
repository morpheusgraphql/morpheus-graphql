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

type ResolverO o e m a = Flexible (Resolver o e m) a

type ComposedResolver o e m f a = Composed (Resolver o e m) f a

type ResolverQ e m a = Flexible (Resolver QUERY e m) a

type ResolverM e m a = Flexible (Resolver MUTATION e m) a

type ResolverS e m a = Flexible (Resolver SUBSCRIPTION e m) a

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

publish :: Monad m => [e] -> Resolver MUTATION e m ()
publish = pushEvents

constRes :: (WithOperation o, Monad m) => b -> a -> Resolver o e m b
constRes = const . pure

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