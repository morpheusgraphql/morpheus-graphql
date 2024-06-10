{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Data.Morpheus.Types.SelectionTree
-- Description : A simple interface for Morpheus internal Selection Set's representation.
module Data.Morpheus.Types.SelectionTree
  ( SelectionTree (..),
  )
where

import Data.Aeson (ToJSON (..), Value)
import Data.Morpheus.Internal.Utils (IsMap (lookup))
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Operation (..),
    Selection (..),
    SelectionContent (..),
    UnionTag (..),
    VALID,
    Variable (..),
    VariableContent (..),
    unpackName,
  )
import Data.Morpheus.Types.Internal.AST.Name (Name)
import Data.Text (unpack)
import Relude hiding (empty)

__lookup :: (IsMap (Name t) m, ToString n) => n -> m a -> Maybe a
__lookup name = lookup (fromString $ toString name)

__argument :: (IsString name) => Argument VALID -> (name, Value)
__argument Argument {..} = (fromString $ toString argumentName, toJSON argumentValue)

__variable :: (IsString name) => Variable VALID -> (name, Value)
__variable Variable {..} = (fromString $ toString variableName, __variableContent variableValue)

__variableContent :: VariableContent VALID -> Value
__variableContent (ValidVariableValue x) = toJSON x

-- | The 'SelectionTree' instance is a simple interface for interacting
-- with morpheus's internal AST while keeping the ability to safely change the concrete
-- representation of the AST.
class SelectionTree node where
  type ChildNode node :: Type

  -- | leaf test: is the list of children empty?
  isLeaf :: node -> Bool

  -- | get a node's name (real name. not alias)
  getName :: (IsString name) => node -> name

  -- | Get the children
  getChildrenList :: node -> [ChildNode node]
  getChildrenList = getChildren

  -- | get the child nodes
  getChildren :: node -> [ChildNode node]

  -- | lookup child node by name (does not use aliases)
  getChild :: (ToString name) => name -> node -> Maybe (ChildNode node)

  -- | checks if the node has a child with the specified name (does not use aliases)
  hasChild :: (ToString name) => name -> node -> Bool
  hasChild name = isJust . getChild name

  -- | get node arguments (as aeson values)
  getArguments :: (IsString name) => node -> [(name, Value)]

  -- | get node argument by name (as aeson values)
  getArgument :: (ToString name) => name -> node -> Maybe Value

instance SelectionTree (Selection VALID) where
  type ChildNode (Selection VALID) = Selection VALID

  isLeaf node = case selectionContent node of
    SelectionField -> True
    _ -> False

  getChildren node = case selectionContent node of
    SelectionField -> mempty
    (SelectionSet deeperSel) -> toList deeperSel
    (UnionSelection interfaceSelection sel) ->
      concatMap toList (toList interfaceSelection)
        <> concatMap
          (toList . unionTagSelection)
          (toList sel)

  getChild name node = case selectionContent node of
    SelectionField -> Nothing
    (SelectionSet deeperSel) -> __lookup name deeperSel
    (UnionSelection interfaceSelection sel) -> select (toList interfaceSelection <> map unionTagSelection (toList sel))
      where
        select (x : xs) = __lookup name x <|> select xs
        select [] = Nothing

  getName :: (IsString name) => Selection VALID -> name
  getName = toName . selectionName

  getArguments = map __argument . toList . selectionArguments

  getArgument name = fmap (toJSON . argumentValue) . __lookup name . selectionArguments

instance SelectionTree (Operation VALID) where
  type ChildNode (Operation VALID) = Selection VALID

  isLeaf _ = False

  getChildren = toList . operationSelection

  getChild name = __lookup name . operationSelection

  getName = toName . fromMaybe "Root" . operationName

  getArguments = map __variable . toList . operationArguments

  getArgument name = fmap (__variableContent . variableValue) . __lookup name . operationArguments

toName :: (IsString name) => Name t -> name
toName = fromString . unpack . unpackName
