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
    vaidateFragmentSelection,
  )
where

import Control.Applicative ((<*>), pure)
import Data.Foldable (null)
import Data.Functor ((<$>), fmap)
import Data.List (filter)
import Data.Maybe (Maybe (..), maybe)
import Data.Morpheus.Error.Selection
  ( hasNoSubfields,
    subfieldsNotSelected,
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    concatTraverse,
    elems,
    empty,
    keyOf,
    singleton,
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
    ValidationError (..),
    isEntNode,
    msgValidation,
    possibleTypes,
    toCategory,
    typed,
    withPosition,
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
import Data.Semigroup ((<>))
import Prelude
  ( ($),
    (&&),
    (.),
    Eq (..),
    const,
    not,
    otherwise,
  )

selectionsWitoutTypename :: SelectionSet VALID -> [Selection VALID]
selectionsWitoutTypename = filter (("__typename" /=) . keyOf) . elems

singleTopLevelSelection :: Operation RAW -> SelectionSet VALID -> SelectionValidator ()
singleTopLevelSelection Operation {operationType = Subscription, operationName} selSet =
  case selectionsWitoutTypename selSet of
    (_ : xs) | not (null xs) -> failure $ fmap (singleTopLevelSelectionError operationName) xs
    _ -> pure ()
singleTopLevelSelection _ _ = pure ()

singleTopLevelSelectionError :: Maybe FieldName -> Selection VALID -> ValidationError
singleTopLevelSelectionError name Selection {selectionPosition} =
  withPosition (Just selectionPosition) $
    subscriptionName
      <> " must select "
      <> "only one top level field."
  where
    subscriptionName = maybe "Anonymous Subscription" (("Subscription " <>) . msgValidation) name

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
  FragmentValidator s (SelectionSet VALID)
processSelectionDirectives location rawDirectives sel = do
  directives <- validateDirectives location rawDirectives
  include <- shouldIncludeSelection directives
  selection <- sel directives
  pure $
    if include
      then selection
      else empty

vaidateFragmentSelection :: (ResolveFragment s) => Fragment RAW -> FragmentValidator s (SelectionSet VALID)
vaidateFragmentSelection f@Fragment {fragmentSelection} = do
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
validateSelectionSet typeDef =
  concatTraverse validateSelection
  where
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
    validateSelection :: Selection RAW -> FragmentValidator s (SelectionSet VALID)
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
        (const $ unionTagSelection <$> resolveValidFragment vaidateFragmentSelection types ref)
    validateSelection
      ( InlineFragment
          fragment@Fragment
            { fragmentDirectives
            }
        ) = do
        types <- possibleTypes typeDef <$> askSchema
        processSelectionDirectives INLINE_FRAGMENT fragmentDirectives $
          const (validate types fragment)
    validate types = fmap fragmentSelection . validateFragment vaidateFragmentSelection types

validateContentLeaf ::
  Ref ->
  TypeDefinition OUT VALID ->
  FragmentValidator s' (SelectionContent s)
validateContentLeaf
  (Ref selectionName selectionPosition)
  TypeDefinition {typeName, typeContent}
    | isEntNode typeContent = pure SelectionField
    | otherwise =
      failure $ subfieldsNotSelected selectionName typeName selectionPosition

validateByTypeContent ::
  forall s.
  (ResolveFragment s) =>
  TypeDefinition OUT VALID ->
  Ref ->
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
          vaidateFragmentSelection
          validateSelectionSet
          unionMembers
      -- Validate Regular selection set
      __validate DataObject {..} =
        fmap SelectionSet . validateSelectionSet (TypeDefinition {typeContent = DataObject {..}, ..})
      -- TODO: Union Like Validation
      __validate DataInterface {..} =
        validateInterfaceSelection
          vaidateFragmentSelection
          validateSelectionSet
          (TypeDefinition {typeContent = DataInterface {..}, ..})
      __validate _ =
        const
          $ failure
          $ hasNoSubfields
            currentSelectionRef
            typeDef
