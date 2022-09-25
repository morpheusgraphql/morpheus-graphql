{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Internal.Graph
  ( cycleChecking,
    Node,
    Graph,
    Edges,
  )
where

import Data.List (lookup)
import Data.Morpheus.Types.Internal.AST (Ref (..))
import Relude

type Node = Ref

type Edges name = (Ref name, [Ref name])

type Graph name = [Edges name]

cycleChecking ::
  (Eq name, Monad m) =>
  (NonEmpty (Ref name) -> m ()) ->
  Graph name ->
  m ()
cycleChecking fail' graph = traverse_ checkNode graph
  where
    checkNode (node, _) = cycleCheckingWith graph node [node] fail'

cycleCheckingWith ::
  (Eq name, Monad m) =>
  Graph name ->
  Ref name ->
  [Ref name] ->
  (NonEmpty (Ref name) -> m ()) ->
  m ()
cycleCheckingWith graph parentNode history fail' = forM_ (lookup parentNode graph) (traverse_ checkNode)
  where
    checkNode node
      | node `elem` history =
          fail' (node :| history)
      | otherwise =
          cycleCheckingWith
            graph
            node
            (history <> [node])
            fail'
