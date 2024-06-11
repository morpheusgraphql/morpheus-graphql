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
    throwErrors,
  )
import Data.Morpheus.Types.Internal.AST
  ( Arguments,
    DirectiveLocation (..),
    Directives,
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    Fragment (..),
    FragmentName,
    GQLError,
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
    at,
    isLeaf,
    mkTypeRef,
    msg,
    possibleTypes,
    toCategory,
    typed,
  )
import Data.Morpheus.Types.Internal.Validation
  ( FragmentValidator,
    SelectionValidator,
    askType,
    getOperationType,
    schema,
    selectKnown,
    setSelection,
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
  ( ValidateFragmentSelection,
    selectFragmentType,
    validateFragment,
    validateSpread,
  )
import Data.Morpheus.Validation.Query.UnionSelection
  ( validateInterfaceSelection,
    validateUnionSelection,
  )
import Relude hiding (empty, join)

selectionsWithoutTypename :: SelectionSet VALID -> [Selection VALID]
selectionsWithoutTypename = filter (("__typename" /=) . keyOf) . toList

singleTopLevelSelection :: Operation RAW -> SelectionSet VALID -> SelectionValidator ()
singleTopLevelSelection Operation {operationType = OPERATION_SUBSCRIPTION, operationName} selSet =
  case selectionsWithoutTypename selSet of
    (_ : (x : xs)) -> throwErrors $ fmap (singleTopLevelSelectionError operationName) (x :| xs)
    _ -> pure ()
singleTopLevelSelection _ _ = pure ()

singleTopLevelSelectionError :: Maybe FieldName -> Selection VALID -> GQLError
singleTopLevelSelectionError name Selection {selectionPosition} =
  ( maybe "Anonymous Subscription" (("Subscription " <>) . msg) name
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
      pure
        $ Operation
          { operationName,
            operationType,
            operationArguments = empty,
            operationSelection = selection,
            operationDirectives = directives,
            ..
          }

toDirectiveLocation :: OperationType -> DirectiveLocation
toDirectiveLocation OPERATION_SUBSCRIPTION = LOCATION_SUBSCRIPTION
toDirectiveLocation OPERATION_MUTATION = LOCATION_MUTATION
toDirectiveLocation OPERATION_QUERY = LOCATION_QUERY

processSelectionDirectives ::
  DirectiveLocation ->
  Directives RAW ->
  (Directives VALID -> FragmentValidator s (SelectionSet VALID)) ->
  FragmentValidator s (Maybe (SelectionSet VALID))
processSelectionDirectives location rawDirectives sel = do
  directives <- validateDirectives location rawDirectives
  include <- shouldIncludeSelection directives
  selection <- sel directives
  pure
    $ if include
      then Just selection
      else Nothing

validateFragmentSelection :: (ValidateFragmentSelection s) => Fragment RAW -> FragmentValidator s (SelectionSet VALID)
validateFragmentSelection f@Fragment {fragmentSelection} = do
  typeDef <- selectFragmentType f
  validateSelectionSet typeDef fragmentSelection

getFields :: TypeDefinition IMPLEMENTABLE s -> FieldsDefinition OUT s
getFields TypeDefinition {typeContent = DataObject {objectFields}} = objectFields
getFields TypeDefinition {typeContent = DataInterface fields} = fields

validateSelectionSet ::
  (ValidateFragmentSelection s) =>
  TypeDefinition IMPLEMENTABLE VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionSet VALID)
validateSelectionSet typeDef =
  traverse (validateSelection typeDef)
    . toList
    >=> toNonEmpty
    . catMaybes
    >=> startHistory
    . mergeConcat

-- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
validateSelection :: (ValidateFragmentSelection s) => TypeDefinition IMPLEMENTABLE VALID -> Selection RAW -> FragmentValidator s (Maybe (SelectionSet VALID))
validateSelection typeDef Selection {..} =
  withScope (setSelection typeDef selectionRef)
    $ processSelectionDirectives LOCATION_FIELD selectionDirectives validateContent
  where
    selectionRef = Ref selectionName selectionPosition
    validateContent directives = do
      (validArgs, content) <- validateSelectionContent typeDef selectionRef selectionArguments selectionContent
      let selection =
            Selection
              { selectionArguments = validArgs,
                selectionDirectives = directives,
                selectionContent = content,
                ..
              }
      pure $ singleton (keyOf selection) selection
validateSelection typeDef (Spread dirs ref) =
  processSelectionDirectives LOCATION_FRAGMENT_SPREAD dirs
    $ const
    $ validateSpreadSelection typeDef ref
validateSelection typeDef (InlineFragment fragment@Fragment {fragmentDirectives}) =
  processSelectionDirectives LOCATION_INLINE_FRAGMENT fragmentDirectives
    $ const
    $ validateInlineFragmentSelection typeDef fragment

validateSpreadSelection ::
  (ValidateFragmentSelection s) =>
  TypeDefinition a VALID ->
  Ref FragmentName ->
  FragmentValidator s (SelectionSet VALID)
validateSpreadSelection typeDef ref = do
  types <- possibleTypes typeDef <$> asks schema
  unionTagSelection <$> validateSpread validateFragmentSelection types ref

validateInlineFragmentSelection ::
  (ValidateFragmentSelection s) =>
  TypeDefinition IMPLEMENTABLE VALID ->
  Fragment RAW ->
  FragmentValidator s (SelectionSet VALID)
validateInlineFragmentSelection typeDef x = do
  types <- possibleTypes typeDef <$> asks schema
  fragmentSelection <$> validateFragment LOCATION_INLINE_FRAGMENT validateFragmentSelection types x

selectSelectionField ::
  Ref FieldName ->
  TypeDefinition IMPLEMENTABLE s ->
  FragmentValidator s' (FieldDefinition OUT s)
selectSelectionField ref typeDef
  | refName ref == "__typename" =
      pure
        FieldDefinition
          { fieldDescription = Nothing,
            fieldName = "__typename",
            fieldType = mkTypeRef "String",
            fieldContent = Nothing,
            fieldDirectives = empty
          }
  | otherwise = selectKnown ref (getFields typeDef)

validateSelectionContent ::
  forall s.
  (ValidateFragmentSelection s) =>
  TypeDefinition IMPLEMENTABLE VALID ->
  Ref FieldName ->
  Arguments RAW ->
  SelectionContent RAW ->
  FragmentValidator s (Arguments VALID, SelectionContent VALID)
validateSelectionContent typeDef ref selectionArguments content = do
  fieldDef <- selectSelectionField ref typeDef
  fieldTypeDef <- askType (typed fieldType fieldDef)
  validArgs <- validateFieldArguments fieldDef selectionArguments
  validContent <- validateContent fieldTypeDef content
  pure (validArgs, validContent)
  where
    validateContent fieldTypeDef SelectionField = validateContentLeaf ref fieldTypeDef
    validateContent fieldTypeDef (SelectionSet rawSelectionSet) = validateByTypeContent fieldTypeDef ref rawSelectionSet

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
  (ValidateFragmentSelection s) =>
  TypeDefinition OUT VALID ->
  Ref FieldName ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateByTypeContent
  typeDef@TypeDefinition {typeContent, ..}
  currentSelectionRef =
    withScope (setSelection typeDef currentSelectionRef)
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
