{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Data.Morpheus.Types.SelectionTree
-- Description : A simple interface for Morpheus internal Selection Set's representation.
module Data.Morpheus.Types.SelectionTree where

import Data.Morpheus.Internal.Utils
  ( empty,
    keyOf,
  )
import Data.Morpheus.Types.Internal.AST
  ( Operation (..),
    Selection (..),
    SelectionContent (..),
    UnionTag (..),
    VALID,
    unpackName,
  )
import Data.Text (unpack)
import Relude hiding (empty)

operationSelectionTree :: Operation s -> Selection s
operationSelectionTree Operation {..} =
  Selection
    { selectionName = fromMaybe "Root" operationName,
      selectionArguments = empty,
      selectionPosition = operationPosition,
      selectionAlias = Nothing,
      selectionContent = SelectionSet operationSelection,
      selectionDirectives = empty
    }

-- | The 'SelectionTree' instance is a simple interface for interacting
-- with morpheus's internal AST while keeping the ability to safely change the concrete
-- representation of the AST.
-- The set of operation is very limited on purpose.
class SelectionTree nodeType where
  -- | leaf test: is the list of children empty?
  isLeaf :: nodeType -> Bool

  -- | Get the children
  getChildrenList :: nodeType -> [nodeType]

  -- | get a node's name
  getName :: IsString name => nodeType -> name

instance SelectionTree (Selection VALID) where
  isLeaf node = case selectionContent node of
    SelectionField -> True
    _ -> False

  getChildrenList node = case selectionContent node of
    SelectionField -> mempty
    (SelectionSet deeperSel) -> toList deeperSel
    (UnionSelection interfaceSelection sel) ->
      toList interfaceSelection
        <> concatMap
          (toList . unionTagSelection)
          (toList sel)

  getName =
    fromString
      . unpack
      . unpackName
      . keyOf
