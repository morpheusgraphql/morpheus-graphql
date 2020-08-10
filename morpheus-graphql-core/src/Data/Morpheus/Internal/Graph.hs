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

import Control.Applicative (Applicative, pure)
import Data.Foldable (traverse_)
import Data.List (elem, lookup)
import Data.Maybe (Maybe (..))
-- MORPHEUS
import Data.Morpheus.Types.Internal.AST
  ( Ref (..),
  )
import Data.Semigroup ((<>))
import Prelude
  ( otherwise,
  )

type Node = Ref

type Edges = (Ref, [Ref])

type Graph = [Edges]

cycleChecking ::
  Applicative m =>
  ([Ref] -> m ()) ->
  Graph ->
  m ()
cycleChecking fail graph = traverse_ checkNode graph
  where
    checkNode (node, _) = cycleCheckingWith graph node [node] fail

cycleCheckingWith ::
  Applicative m =>
  Graph ->
  Ref ->
  [Ref] ->
  ([Ref] -> m ()) ->
  m ()
cycleCheckingWith graph parentNode history fail =
  case lookup parentNode graph of
    Just node -> traverse_ checkNode node
    Nothing -> pure ()
  where
    checkNode node
      | node `elem` history =
        fail (node : history)
      | otherwise =
        cycleCheckingWith
          graph
          node
          (history <> [node])
          fail
