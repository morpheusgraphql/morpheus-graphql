{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Resolving
  ( Event (..),
    Resolver,
    LiftOperation,
    runRootResModel,
    lift,
    SubEvent,
    Eventless,
    Failure (..),
    eventChannels,
    ResponseEvent (..),
    ResponseStream,
    cleanEvents,
    Result (..),
    ResultT (..),
    unpackEvents,
    ObjectResModel (..),
    ResModel (..),
    FieldResModel,
    WithOperation,
    PushEvents (..),
    subscribe,
    ResolverContext (..),
    unsafeInternalContext,
    RootResModel (..),
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
    Channel (..),
    ResolverState,
    liftResolverState,
    mkValue,
  )
where

import Control.Applicative (pure)
import Control.Monad (Monad)
import qualified Data.Aeson as A
import Data.Functor (fmap)
import qualified Data.HashMap.Strict as HM
  ( lookup,
    toList,
  )
import Data.Maybe (maybe)
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
import qualified Data.Vector as V
  ( toList,
  )
import Prelude
  ( ($),
    (.),
    Bool,
    Float,
    Int,
  )

mkString :: Token -> ResModel o e m
mkString = ResScalar . String

mkFloat :: Float -> ResModel o e m
mkFloat = ResScalar . Float

mkInt :: Int -> ResModel o e m
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResModel o e m
mkBoolean = ResScalar . Boolean

mkEnum :: TypeName -> TypeName -> ResModel o e m
mkEnum = ResEnum

mkList :: [ResModel o e m] -> ResModel o e m
mkList = ResList

mkUnion :: TypeName -> Resolver o e m (ResModel o e m) -> ResModel o e m
mkUnion = ResUnion

mkNull :: ResModel o e m
mkNull = ResNull

unPackName :: A.Value -> TypeName
unPackName (A.String x) = TypeName x
unPackName _ = "__JSON__"

mkValue ::
  (LiftOperation o, Monad m) =>
  A.Value ->
  ResModel o e m
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

mkObject ::
  TypeName ->
  [(FieldName, Resolver o e m (ResModel o e m))] ->
  ResModel o e m
mkObject __typename objectFields =
  ResObject
    ( ObjectResModel
        { __typename,
          objectFields
        }
    )
