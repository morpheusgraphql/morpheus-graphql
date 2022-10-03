{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Data.Morpheus.Types.SelectionTree
-- Description : A simple interface for Morpheus internal Selection Set's representation.
module Data.Morpheus.Types.SelectionTree
  ( SelectionTree (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( Operation (..),
    Selection (..),
    SelectionContent (..),
    UnionTag (..),
    VALID,
    unpackName,
  )
import Data.Morpheus.Types.Internal.AST.Name (Name)
import Data.Text (unpack)
import Relude hiding (empty)

-- | The 'SelectionTree' instance is a simple interface for interacting
-- with morpheus's internal AST while keeping the ability to safely change the concrete
-- representation of the AST.
-- The set of operation is very limited on purpose.
class SelectionTree (ChildNode node) => SelectionTree node where
  type ChildNode node :: Type

  -- | leaf test: is the list of children empty?
  isLeaf :: node -> Bool

  -- | get a node's name (real name. not alias)
  getName :: IsString name => node -> name

  -- | Get the children
  getChildrenList :: node -> [ChildNode node]
  getChildrenList = getNodes

  -- | get the child nodes
  getNodes :: node -> [ChildNode node]

  -- | lookup child node by name (does not use aliases)
  lookupNode :: String -> node -> Maybe (ChildNode node)
  lookupNode name node = do
    let selections = getChildrenList node
    find ((name ==) . getName) selections

  -- | checks if the node has a child with the specified name (does not use aliases)
  memberNode :: String -> node -> Bool
  memberNode name = isJust . lookupNode name

instance SelectionTree (Selection VALID) where
  type ChildNode (Selection VALID) = Selection VALID

  isLeaf node = case selectionContent node of
    SelectionField -> True
    _ -> False

  getNodes node = case selectionContent node of
    SelectionField -> mempty
    (SelectionSet deeperSel) -> toList deeperSel
    (UnionSelection interfaceSelection sel) ->
      toList interfaceSelection
        <> concatMap
          (toList . unionTagSelection)
          (toList sel)

  getName = toName . selectionName

instance SelectionTree (Operation VALID) where
  type ChildNode (Operation VALID) = Selection VALID

  isLeaf _ = False

  getNodes = toList . operationSelection

  getName = toName . fromMaybe "Root" . operationName

toName :: IsString name => Name t -> name
toName = fromString . unpack . unpackName
