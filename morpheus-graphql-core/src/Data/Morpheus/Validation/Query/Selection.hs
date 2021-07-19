{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Query.Selection
  ( validateOperation,
    validateFragmentSelection,
  )
where

import Control.Monad.Except (throwError)
import Data.Mergeable
  ( toNonEmpty,
  )
import Data.Morpheus.Error.Selection
  ( hasNoSubfields,
    subfieldsNotSelected,
  )
import Data.Morpheus.Ext.Empty (Empty (..))
import Data.Morpheus.Internal.Utils
  ( keyOf,
    mergeConcat,
    singleton,
    startHistory,
    throwMany,
  )
import Data.Morpheus.Types.Internal.AST
  ( Arguments,
    DirectiveLocation (FIELD, FRAGMENT_SPREAD, INLINE_FRAGMENT, MUTATION, QUERY, SUBSCRIPTION),
    Directives,
    FieldDefinition (fieldType),
    FieldName,
    FieldsDefinition,
    Fragment (..),
    IMPLEMENTABLE,
    OUT,
    Operation (..),
    OperationType (..),
    RAW,
    Ref (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    UnionTag (..),
    VALID,
    ValidationError,
    at,
    isLeaf,
    msgValidation,
    possibleTypes,
    toCategory,
    typed,
  )
import Data.Morpheus.Types.Internal.Validation
  ( FragmentValidator,
    SelectionValidator,
    askSchema,
    askType,
    getOperationType,
    selectKnown,
    withScope,
  )
import Data.Morpheus.Validation.Internal.Arguments
  ( validateFieldArguments,
  )
import Data.Morpheus.Validation.Internal.Directive
  ( shouldIncludeSelection,
    validateDirectives,
  )
import Data.Morpheus.Validation.Query.Fragment
  ( ResolveFragment (..),
    resolveValidFragment,
    selectFragmentType,
    validateFragment,
  )
import Data.Morpheus.Validation.Query.UnionSelection
  ( validateInterfaceSelection,
    validateUnionSelection,
  )
import Relude hiding (empty, join)

selectionsWithoutTypename :: SelectionSet VALID -> [Selection VALID]
selectionsWithoutTypename = filter (("__typename" /=) . keyOf) . toList

singleTopLevelSelection :: Operation RAW -> SelectionSet VALID -> SelectionValidator ()
singleTopLevelSelection Operation {operationType = Subscription, operationName} selSet =
  case selectionsWithoutTypename selSet of
    (_ : (x : xs)) -> throwMany $ fmap (singleTopLevelSelectionError operationName) (x :| xs)
    _ -> pure ()
singleTopLevelSelection _ _ = pure ()

singleTopLevelSelectionError :: Maybe FieldName -> Selection VALID -> ValidationError
singleTopLevelSelectionError name Selection {selectionPosition} =
  ( maybe "Anonymous Subscription" (("Subscription " <>) . msgValidation) name
      <> " must select "
      <> "only one top level field."
  )
    `at` selectionPosition

validateOperation ::
  Operation RAW ->
  SelectionValidator (Operation VALID)
validateOperation
  rawOperation@Operation
    { operationName,
      operationType,
      operationSelection,
      operationDirectives,
      ..
    } =
    do
      typeDef <- getOperationType rawOperation
      selection <- validateSelectionSet (toCategory typeDef) operationSelection
      singleTopLevelSelection rawOperation selection
      directives <-
        validateDirectives
          (toDirectiveLocation operationType)
          operationDirectives
      pure $
        Operation
          { operationName,
            operationType,
            operationArguments = empty,
            operationSelection = selection,
            operationDirectives = directives,
            ..
          }

toDirectiveLocation :: OperationType -> DirectiveLocation
toDirectiveLocation Subscription = SUBSCRIPTION
toDirectiveLocation Mutation = MUTATION
toDirectiveLocation Query = QUERY

processSelectionDirectives ::
  DirectiveLocation ->
  Directives RAW ->
  (Directives VALID -> FragmentValidator s (SelectionSet VALID)) ->
  FragmentValidator s (Maybe (SelectionSet VALID))
processSelectionDirectives location rawDirectives sel = do
  directives <- validateDirectives location rawDirectives
  include <- shouldIncludeSelection directives
  selection <- sel directives
  pure $
    if include
      then Just selection
      else Nothing

validateFragmentSelection :: (ResolveFragment s) => Fragment RAW -> FragmentValidator s (SelectionSet VALID)
validateFragmentSelection f@Fragment {fragmentSelection} = do
  typeDef <- selectFragmentType f
  validateSelectionSet typeDef fragmentSelection

getFields :: TypeDefinition IMPLEMENTABLE s -> FieldsDefinition OUT s
getFields TypeDefinition {typeContent = DataObject {objectFields}} = objectFields
getFields TypeDefinition {typeContent = DataInterface fields} = fields

validateSelectionSet ::
  forall s.
  ( ResolveFragment s
  ) =>
  TypeDefinition IMPLEMENTABLE VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionSet VALID)
validateSelectionSet typeDef selectionSet =
  traverse validateSelection (toList selectionSet)
    >>= toNonEmpty . catMaybes
    >>= startHistory . mergeConcat
  where
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
    validateSelection :: Selection RAW -> FragmentValidator s (Maybe (SelectionSet VALID))
    validateSelection
      sel@Selection
        { selectionName,
          selectionArguments,
          selectionContent,
          selectionPosition,
          selectionDirectives
        } =
        withScope
          typeDef
          currentSelectionRef
          $ processSelectionDirectives
            FIELD
            selectionDirectives
            (`validateSelectionContent` selectionContent)
        where
          currentSelectionRef = Ref selectionName selectionPosition
          commonValidation :: FragmentValidator s (TypeDefinition OUT VALID, Arguments VALID)
          commonValidation = do
            fieldDef <- selectKnown (Ref selectionName selectionPosition) (getFields typeDef)
            (,)
              <$> askType (typed fieldType fieldDef)
              <*> validateFieldArguments fieldDef selectionArguments
          -----------------------------------------------------------------------------------
          validateSelectionContent :: Directives VALID -> SelectionContent RAW -> FragmentValidator s (SelectionSet VALID)
          validateSelectionContent directives SelectionField
            | null selectionArguments && selectionName == "__typename" =
              pure $ singleton $
                sel
                  { selectionArguments = empty,
                    selectionDirectives = directives,
                    selectionContent = SelectionField
                  }
            | otherwise = do
              (datatype, validArgs) <- commonValidation
              selContent <-
                validateContentLeaf currentSelectionRef datatype
              pure $
                singleton
                  ( sel
                      { selectionArguments = validArgs,
                        selectionDirectives = directives,
                        selectionContent = selContent
                      }
                  )
          ----- SelectionSet
          validateSelectionContent directives (SelectionSet rawSelectionSet) =
            do
              (tyDef, validArgs) <- commonValidation
              selContent <- validateByTypeContent tyDef currentSelectionRef rawSelectionSet
              pure $ singleton $
                sel
                  { selectionArguments = validArgs,
                    selectionDirectives = directives,
                    selectionContent = selContent
                  }
    validateSelection (Spread dirs ref) = do
      types <- possibleTypes typeDef <$> askSchema
      processSelectionDirectives
        FRAGMENT_SPREAD
        dirs
        (const $ unionTagSelection <$> resolveValidFragment validateFragmentSelection types ref)
    validateSelection
      ( InlineFragment
          fragment@Fragment
            { fragmentDirectives
            }
        ) = do
        types <- possibleTypes typeDef <$> askSchema
        processSelectionDirectives INLINE_FRAGMENT fragmentDirectives $
          const (validate types fragment)
    validate types = fmap fragmentSelection . validateFragment validateFragmentSelection types

validateContentLeaf ::
  Ref FieldName ->
  TypeDefinition OUT VALID ->
  FragmentValidator s' (SelectionContent s)
validateContentLeaf
  (Ref selectionName selectionPosition)
  TypeDefinition {typeName, typeContent}
    | isLeaf typeContent = pure SelectionField
    | otherwise =
      throwError $ subfieldsNotSelected selectionName typeName selectionPosition

validateByTypeContent ::
  forall s.
  (ResolveFragment s) =>
  TypeDefinition OUT VALID ->
  Ref FieldName ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateByTypeContent
  typeDef@TypeDefinition {typeContent, ..}
  currentSelectionRef =
    withScope typeDef currentSelectionRef
      . __validate typeContent
    where
      __validate ::
        TypeContent TRUE OUT VALID ->
        SelectionSet RAW ->
        FragmentValidator s (SelectionContent VALID)
      -- Validate UnionSelection
      __validate DataUnion {unionMembers} =
        validateUnionSelection
          validateFragmentSelection
          validateSelectionSet
          unionMembers
      -- Validate Regular selection set
      __validate DataObject {..} =
        fmap SelectionSet . validateSelectionSet (TypeDefinition {typeContent = DataObject {..}, ..})
      -- TODO: Union Like Validation
      __validate DataInterface {..} =
        validateInterfaceSelection
          validateFragmentSelection
          validateSelectionSet
          (TypeDefinition {typeContent = DataInterface {..}, ..})
      __validate _ =
        const
          $ throwError
          $ hasNoSubfields
            currentSelectionRef
            typeDef
