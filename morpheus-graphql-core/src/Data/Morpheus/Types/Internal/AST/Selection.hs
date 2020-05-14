{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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

import Data.Maybe (fromMaybe, isJust)
-- MORPHEUS

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
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    GQLError (..),
    GQLErrors,
    Message,
    OperationType (..),
    Position,
    RAW,
    Ref (..),
    Stage,
    TypeName (..),
    VALID,
    intercalateName,
    msg,
    readName,
  )
import Data.Morpheus.Types.Internal.AST.Data
  ( Arguments,
    OUT,
    Schema (..),
    TypeDefinition (..),
  )
import Data.Morpheus.Types.Internal.AST.MergeSet
  ( MergeSet,
  )
import Data.Morpheus.Types.Internal.AST.OrderedMap
  ( OrderedMap,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( ResolvedValue,
    Variable (..),
    VariableDefinitions,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH.Syntax (Lift (..))

data Fragment = Fragment
  { fragmentName :: FieldName,
    fragmentType :: TypeName,
    fragmentPosition :: Position,
    fragmentSelection :: SelectionSet RAW
  }
  deriving (Show, Eq, Lift)

-- ERRORs
instance NameCollision Fragment where
  nameCollision _ Fragment {fragmentName, fragmentPosition} =
    GQLError
      { message = "There can be only one fragment named " <> msg fragmentName <> ".",
        locations = [fragmentPosition]
      }

instance KeyOf Fragment where
  keyOf = fragmentName

type Fragments = OrderedMap FieldName Fragment

data SelectionContent (s :: Stage) where
  SelectionField :: SelectionContent s
  SelectionSet :: SelectionSet s -> SelectionContent s
  UnionSelection :: UnionSelection -> SelectionContent VALID

instance Merge (SelectionContent s) where
  merge path (SelectionSet s1) (SelectionSet s2) = SelectionSet <$> merge path s1 s2
  merge path (UnionSelection u1) (UnionSelection u2) = UnionSelection <$> merge path u1 u2
  merge path oldC currC
    | oldC == currC = pure oldC
    | otherwise =
      failure
        [ GQLError
            { message = msg (intercalateName "." $ map refName path),
              locations = map refPosition path
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

mergeConflict :: [Ref] -> GQLError -> GQLErrors
mergeConflict [] err = [err]
mergeConflict refs@(rootField : xs) err =
  [ GQLError
      { message = renderSubfields <> message err,
        locations = map refPosition refs <> locations err
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

instance KeyOf UnionTag where
  type KEY UnionTag = TypeName
  keyOf = unionTagName

type UnionSelection = MergeSet UnionTag

type SelectionSet s = MergeSet (Selection s)

data Selection (s :: Stage) where
  Selection ::
    { selectionName :: FieldName,
      selectionAlias :: Maybe FieldName,
      selectionPosition :: Position,
      selectionArguments :: Arguments s,
      selectionContent :: SelectionContent s
    } ->
    Selection s
  InlineFragment :: Fragment -> Selection RAW
  Spread :: Ref -> Selection RAW

instance KeyOf (Selection s) where
  keyOf Selection {selectionName, selectionAlias} = fromMaybe selectionName selectionAlias
  keyOf InlineFragment {} = ""
  keyOf Spread {} = ""

useDufferentAliases :: Message
useDufferentAliases =
  "Use different aliases on the "
    <> "fields to fetch both if this was intentional."

instance Merge (Selection a) where
  merge path old@Selection {selectionPosition = pos1} current@Selection {selectionPosition = pos2} =
    do
      selectionName <- mergeName
      let currentPath = path <> [Ref selectionName pos1]
      selectionArguments <- mergeArguments currentPath
      selectionContent <- merge currentPath (selectionContent old) (selectionContent current)
      pure $
        Selection
          { selectionName,
            selectionAlias = mergeAlias,
            selectionPosition = pos1,
            selectionArguments,
            selectionContent
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
            GQLError
              { message =
                  "" <> msg (selectionName old) <> " and " <> msg (selectionName current)
                    <> " are different fields. "
                    <> useDufferentAliases,
                locations = [pos1, pos2]
              }
      ---------------------
      -- allias name is relevant only if they collide by allias like:
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
            GQLError
              { message = "they have differing arguments. " <> useDufferentAliases,
                locations = [pos1, pos2]
              }
  -- TODO:
  merge path old current =
    failure $ mergeConflict path $
      GQLError
        { message = "can't merge. " <> useDufferentAliases,
          locations = map selectionPosition [old, current]
        }

deriving instance Show (Selection a)

deriving instance Lift (Selection a)

deriving instance Eq (Selection a)

type DefaultValue = Maybe ResolvedValue

data Operation (s :: Stage) = Operation
  { operationName :: Maybe FieldName,
    operationType :: OperationType,
    operationArguments :: VariableDefinitions s,
    operationSelection :: SelectionSet s,
    operationPosition :: Position
  }
  deriving (Show, Lift)

getOperationName :: Maybe FieldName -> TypeName
getOperationName = maybe "AnonymousOperation" (TypeName . readName)

getOperationDataType :: Failure GQLErrors m => Operation a -> Schema -> m (TypeDefinition OUT)
getOperationDataType Operation {operationType = Query} lib = pure (query lib)
getOperationDataType Operation {operationType = Mutation, operationPosition} lib =
  case mutation lib of
    Just x -> pure x
    Nothing -> failure $ mutationIsNotDefined operationPosition
getOperationDataType Operation {operationType = Subscription, operationPosition} lib =
  case subscription lib of
    Just x -> pure x
    Nothing -> failure $ subscriptionIsNotDefined operationPosition
