module Data.Morpheus.App.Internal.Resolving.NamedResolver
  ( runResolverMap,
  )
where

import Data.Morpheus.App.Internal.Resolving.Event (EventHandler (Channel))
import Data.Morpheus.App.Internal.Resolving.ResolveValue
  ( resolveRef,
  )
import Data.Morpheus.App.Internal.Resolving.Resolver (LiftOperation, Resolver, ResponseStream, runResolver)
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverState,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolverRef (..),
    ResolverMap,
  )
import Data.Morpheus.Types.Internal.AST
  ( Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeName,
    VALID,
    ValidValue,
    Value (..),
  )

runResolverMap ::
  (Monad m, LiftOperation o) =>
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  TypeName ->
  ResolverMap (Resolver o e m) ->
  ResolverContext ->
  SelectionSet VALID ->
  ResponseStream e m ValidValue
runResolverMap
  channels
  name
  res
  ctx
  selection = runResolver channels resolvedValue ctx
    where
      resolvedValue = resolveRef res (NamedResolverRef name Null) (SelectionSet selection)
