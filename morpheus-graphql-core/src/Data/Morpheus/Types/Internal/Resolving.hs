{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Types.Internal.Resolving
  ( Event (..),
    UnSubResolver,
    Resolver,
    MapStrategy (..),
    LiftOperation,
    runRootResModel,
    toResolver,
    lift,
    SubEvent,
    Eventless,
    Failure (..),
    GQLChannel (..),
    ResponseEvent (..),
    ResponseStream,
    cleanEvents,
    Result (..),
    ResultT (..),
    unpackEvents,
    LibUpdater,
    resolveUpdates,
    setTypeName,
    ObjectResModel (..),
    ResModel (..),
    FieldResModel,
    WithOperation,
    PushEvents (..),
    subscribe,
    Context (..),
    unsafeInternalContext,
    RootResModel (..),
    unsafeBind,
    liftStateless,
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
  )
where

import Data.Morpheus.Types.Internal.AST
  ( Name,
    ScalarValue (..),
    Token,
    TypeName,
  )
import Data.Morpheus.Types.Internal.Resolving.Core
import Data.Morpheus.Types.Internal.Resolving.Resolver

mkString :: Token -> ResModel o e m
mkString = ResScalar . String

mkFloat :: Float -> ResModel o e m
mkFloat = ResScalar . Float

mkInt :: Int -> ResModel o e m
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResModel o e m
mkBoolean = ResScalar . Boolean

mkEnum :: TypeName -> Name -> ResModel o e m
mkEnum = ResEnum

mkList :: [ResModel o e m] -> ResModel o e m
mkList = ResList

mkUnion :: TypeName -> Resolver o e m (ResModel o e m) -> ResModel o e m
mkUnion = ResUnion

mkNull :: ResModel o e m
mkNull = ResNull

mkObject ::
  TypeName ->
  [(Name, Resolver o e m (ResModel o e m))] ->
  ResModel o e m
mkObject __typename objectFields =
  ResObject
    ( ObjectResModel
        { __typename,
          objectFields
        }
    )
