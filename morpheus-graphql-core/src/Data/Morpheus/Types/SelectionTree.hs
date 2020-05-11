{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Morpheus.Types.SelectionTree
-- Description : A simple interface for Morpheus internal Selection Set's representation.
module Data.Morpheus.Types.SelectionTree where

import Data.Maybe (Maybe)
import Data.Morpheus.Types.Internal.AST.Base (VALID)
import Data.Morpheus.Types.Internal.AST.MergeSet (unpack)
import Data.Morpheus.Types.Internal.AST.Selection
  ( Selection (selectionContent, selectionName),
    SelectionContent (SelectionField, SelectionSet),
  )
import Data.Text (Text)

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
  getName :: nodeType -> Text

instance SelectionTree (Selection VALID) where
  isLeaf node = case selectionContent node of
    SelectionField -> True
    _ -> False
  getChildrenList node = case selectionContent node of
    SelectionField -> mempty
    (SelectionSet deeperSel) -> unpack deeperSel
  getName = selectionName
