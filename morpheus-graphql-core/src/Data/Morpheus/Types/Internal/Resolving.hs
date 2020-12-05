{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Resolving
  ( Resolver,
    LiftOperation,
    runRootResolverValue,
    lift,
    Eventless,
    Failure (..),
    ResponseEvent (..),
    ResponseStream,
    cleanEvents,
    Result (..),
    ResultT (..),
    unpackEvents,
    ResolverObject (..),
    ResolverValue (..),
    WithOperation,
    PushEvents (..),
    subscribe,
    ResolverContext (..),
    unsafeInternalContext,
    RootResolverValue (..),
    resultOr,
    withArguments,
    -- Dynamic Resolver
    mkBoolean,
    mkFloat,
    mkInt,
    mkEnum,
    mkList,
    mkUnion,
    mkObject,
    mkNull,
    mkString,
    SubscriptionField (..),
    getArguments,
    ResolverState,
    liftResolverState,
    mkValue,
    ResolverEntry,
    sortErrors,
    EventHandler (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Internal.Utils
  ( mapTuple,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    ScalarValue (..),
    Token,
    TypeName (..),
    decodeScientific,
  )
import Data.Morpheus.Types.Internal.Resolving.Core
import Data.Morpheus.Types.Internal.Resolving.Event
import Data.Morpheus.Types.Internal.Resolving.Resolver
import Data.Morpheus.Types.Internal.Resolving.ResolverState
import Data.Morpheus.Types.Internal.Resolving.ResolverValue
import Data.Morpheus.Types.Internal.Resolving.RootResolverValue
import qualified Data.Vector as V
  ( toList,
  )
import Relude

mkString :: Token -> ResolverValue m
mkString = ResScalar . String

mkFloat :: Double -> ResolverValue m
mkFloat = ResScalar . Float

mkInt :: Int -> ResolverValue m
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResolverValue m
mkBoolean = ResScalar . Boolean

mkList :: [ResolverValue m] -> ResolverValue m
mkList = ResList

mkNull :: ResolverValue m
mkNull = ResNull

unPackName :: A.Value -> TypeName
unPackName (A.String x) = TypeName x
unPackName _ = "__JSON__"

mkValue ::
  (Monad m) =>
  A.Value ->
  ResolverValue m
mkValue (A.Object v) =
  mkObject
    (maybe "__JSON__" unPackName $ HM.lookup "__typename" v)
    $ fmap
      (mapTuple FieldName (pure . mkValue))
      (HM.toList v)
mkValue (A.Array ls) = mkList (fmap mkValue (V.toList ls))
mkValue A.Null = mkNull
mkValue (A.Number x) = ResScalar (decodeScientific x)
mkValue (A.String x) = ResScalar (String x)
mkValue (A.Bool x) = ResScalar (Boolean x)
