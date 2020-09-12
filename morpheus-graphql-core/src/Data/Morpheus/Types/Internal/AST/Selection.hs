{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Selection
  ( Selection (..),
    SelectionContent (..),
    SelectionSet,
    UnionTag (..),
    UnionSelection,
    Fragment (..),
    Fragments,
    Operation (..),
    Variable (..),
    VariableDefinitions,
    DefaultValue,
    getOperationName,
    getOperationDataType,
  )
where

import Control.Applicative (pure)
import Data.Foldable (all, foldr)
import Data.Functor ((<$>), fmap)
import Data.List (filter)
import Data.Maybe (Maybe (..), fromMaybe, isJust, maybe)
import Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
import Data.Morpheus.Error.Operation
  ( mutationIsNotDefined,
    subscriptionIsNotDefined,
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    KeyOf (..),
    Merge (..),
    elems,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
    renderArguments,
    renderObject,
    space,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    Message,
    Msg (..),
    OperationType (..),
    Position,
    Ref (..),
    TypeName (..),
    ValidationError (..),
    ValidationErrors,
    intercalateName,
    msg,
    msgValidation,
    readName,
  )
import Data.Morpheus.Types.Internal.AST.Fields
  ( Argument (..),
    Arguments,
    Directives,
  )
import Data.Morpheus.Types.Internal.AST.MergeSet
  ( MergeSet,
  )
import Data.Morpheus.Types.Internal.AST.OrdMap
  ( OrdMap,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( RAW,
    Stage,
    VALID,
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( OBJECT,
  )
import Data.Morpheus.Types.Internal.AST.TypeSystem
  ( Schema (..),
    TypeDefinition (..),
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( ResolvedValue,
    Value (..),
    Variable (..),
    VariableDefinitions,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude
  ( ($),
    (.),
    Bool (..),
    Eq (..),
    Show (..),
    otherwise,
  )

data Fragment (stage :: Stage) = Fragment
  { fragmentName :: FieldName,
    fragmentType :: TypeName,
    fragmentPosition :: Position,
    fragmentSelection :: SelectionSet stage,
    fragmentDirectives :: Directives stage
  }
  deriving (Show, Eq, Lift)

-- ERRORs
instance NameCollision (Fragment s) where
  nameCollision Fragment {fragmentName, fragmentPosition} =
    ValidationError
      ("There can be only one fragment named " <> msg fragmentName <> ".")
      [fragmentPosition]

instance KeyOf FieldName (Fragment s) where
  keyOf = fragmentName

type Fragments (s :: Stage) = OrdMap FieldName (Fragment s)

data SelectionContent (s :: Stage) where
  SelectionField :: SelectionContent s
  SelectionSet :: SelectionSet s -> SelectionContent s
  UnionSelection :: UnionSelection VALID -> SelectionContent VALID

renderSelectionSet :: SelectionSet VALID -> Rendering
renderSelectionSet = renderObject . elems

instance RenderGQL (SelectionContent VALID) where
  render SelectionField = ""
  render (SelectionSet selSet) = renderSelectionSet selSet
  render (UnionSelection unionSets) = renderObject (elems unionSets)

instance
  Merge (SelectionSet s) =>
  Merge (SelectionContent s)
  where
  merge path (SelectionSet s1) (SelectionSet s2) = SelectionSet <$> merge path s1 s2
  merge path (UnionSelection u1) (UnionSelection u2) = UnionSelection <$> merge path u1 u2
  merge path oldC currentC
    | oldC == currentC = pure oldC
    | otherwise =
      failure
        [ ValidationError
            { validationMessage = msg (intercalateName "." $ fmap refName path),
              validationLocations = fmap refPosition path
            }
        ]

deriving instance Show (SelectionContent a)

deriving instance Eq (SelectionContent a)

deriving instance Lift (SelectionContent a)

data UnionTag = UnionTag
  { unionTagName :: TypeName,
    unionTagSelection :: SelectionSet VALID
  }
  deriving (Show, Eq, Lift)

instance KeyOf TypeName UnionTag where
  keyOf = unionTagName

instance RenderGQL UnionTag where
  render UnionTag {unionTagName, unionTagSelection} =
    "... on "
      <> render unionTagName
      <> space
      <> renderSelectionSet unionTagSelection

mergeConflict :: [Ref] -> ValidationError -> ValidationErrors
mergeConflict [] err = [err]
mergeConflict refs@(rootField : xs) err =
  [ ValidationError
      { validationMessage = renderSubfields <> validationMessage err,
        validationLocations = fmap refPosition refs <> validationLocations err
      }
  ]
  where
    fieldConflicts ref = msg (refName ref) <> " conflict because "
    renderSubfield ref txt = txt <> "subfields " <> fieldConflicts ref
    renderStart = "Fields " <> fieldConflicts rootField
    renderSubfields =
      foldr
        renderSubfield
        renderStart
        xs

instance Merge UnionTag where
  merge path (UnionTag oldTag oldSel) (UnionTag _ currentSel) =
    UnionTag oldTag <$> merge path oldSel currentSel

type UnionSelection (s :: Stage) = MergeSet s UnionTag

type SelectionSet (s :: Stage) = MergeSet s (Selection s)

data Selection (s :: Stage) where
  Selection ::
    { selectionName :: FieldName,
      selectionAlias :: Maybe FieldName,
      selectionPosition :: Position,
      selectionArguments :: Arguments s,
      selectionContent :: SelectionContent s,
      selectionDirectives :: Directives s
    } ->
    Selection s
  InlineFragment :: Fragment RAW -> Selection RAW
  Spread :: Directives RAW -> Ref -> Selection RAW

instance RenderGQL (Selection VALID) where
  render
    Selection
      { ..
      } =
      render (fromMaybe selectionName selectionAlias)
        <> renderArguments (filter notNull (elems selectionArguments))
        <> render selectionContent
      where
        notNull Argument {argumentValue = Null} = False
        notNull _ = True

instance KeyOf FieldName (Selection s) where
  keyOf
    Selection
      { selectionName,
        selectionAlias
      } = fromMaybe selectionName selectionAlias
  keyOf _ = ""

useDifferentAliases :: Message
useDifferentAliases =
  "Use different aliases on the "
    <> "fields to fetch both if this was intentional."

instance
  Merge (SelectionSet a) =>
  Merge (Selection a)
  where
  merge
    path
    old@Selection {selectionPosition = pos1}
    current@Selection {selectionPosition = pos2} =
      do
        selectionName <- mergeName
        let currentPath = path <> [Ref selectionName pos1]
        selectionArguments <- mergeArguments currentPath
        selectionContent <- merge currentPath (selectionContent old) (selectionContent current)
        pure $
          Selection
            { selectionAlias = mergeAlias,
              selectionPosition = pos1,
              selectionDirectives = selectionDirectives old <> selectionDirectives current,
              ..
            }
      where
        -- passes if:

        --     user1: user
        --   }
        -- fails if:

        --     user1: product
        --   }
        mergeName
          | selectionName old == selectionName current = pure $ selectionName current
          | otherwise =
            failure $ mergeConflict path $
              ValidationError
                { validationMessage =
                    "" <> msg (selectionName old) <> " and " <> msg (selectionName current)
                      <> " are different fields. "
                      <> useDifferentAliases,
                  validationLocations = [pos1, pos2]
                }
        ---------------------
        -- alias name is relevant only if they collide by allies like:
        --   { user1: user
        --     user1: user
        --   }
        mergeAlias
          | all (isJust . selectionAlias) [old, current] = selectionAlias old
          | otherwise = Nothing
        --- arguments must be equal
        mergeArguments currentPath
          | selectionArguments old == selectionArguments current = pure $ selectionArguments current
          | otherwise =
            failure $ mergeConflict currentPath $
              ValidationError
                { validationMessage = "they have differing arguments. " <> useDifferentAliases,
                  validationLocations = [pos1, pos2]
                }
  merge path _ _ =
    failure $
      mergeConflict
        path
        ("INTERNAL: can't merge. " <> msgValidation useDifferentAliases :: ValidationError)

deriving instance Show (Selection a)

deriving instance Lift (Selection a)

deriving instance Eq (Selection a)

type DefaultValue = Maybe ResolvedValue

data Operation (s :: Stage) = Operation
  { operationName :: Maybe FieldName,
    operationType :: OperationType,
    operationArguments :: VariableDefinitions s,
    operationSelection :: SelectionSet s,
    operationPosition :: Position,
    operationDirectives :: Directives s
  }
  deriving (Show, Lift)

instance RenderGQL (Operation VALID) where
  render
    Operation
      { operationName,
        operationType,
        operationSelection
      } =
      render operationType
        <> space
        <> render operationName
        <> space
        <> renderSelectionSet operationSelection

getOperationName :: Maybe FieldName -> TypeName
getOperationName = maybe "AnonymousOperation" (TypeName . readName)

getOperationDataType :: Failure ValidationError m => Operation s -> Schema VALID -> m (TypeDefinition OBJECT VALID)
getOperationDataType Operation {operationType = Query} lib = pure (query lib)
getOperationDataType Operation {operationType = Mutation, operationPosition} lib =
  maybe (failure $ mutationIsNotDefined operationPosition) pure (mutation lib)
getOperationDataType Operation {operationType = Subscription, operationPosition} lib =
  maybe (failure $ subscriptionIsNotDefined operationPosition) pure (subscription lib)
