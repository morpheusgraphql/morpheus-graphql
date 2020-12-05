{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Resolving
  ( Resolver,
    LiftOperation,
    runRootResModel,
    lift,
    Eventless,
    Failure (..),
    ResponseEvent (..),
    ResponseStream,
    cleanEvents,
    Result (..),
    ResultT (..),
    unpackEvents,
    ObjectResModel (..),
    ResModel (..),
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
    ResolverState,
    liftResolverState,
    mkValue,
    FieldResModel,
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
import qualified Data.Vector as V
  ( toList,
  )
import Relude

mkString :: Token -> ResModel m
mkString = ResScalar . String

mkFloat :: Double -> ResModel m
mkFloat = ResScalar . Float

mkInt :: Int -> ResModel m
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResModel m
mkBoolean = ResScalar . Boolean

mkList :: [ResModel m] -> ResModel m
mkList = ResList

mkNull :: ResModel m
mkNull = ResNull

unPackName :: A.Value -> TypeName
unPackName (A.String x) = TypeName x
unPackName _ = "__JSON__"

mkValue ::
  (Monad m) =>
  A.Value ->
  ResModel m
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
