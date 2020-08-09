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
    TypeName,
    UnionTag (..),
    VALID,
    ValidationError (..),
    isEntNode,
    msgValidation,
    typed,
    withPosition,
  )
import Data.Morpheus.Types.Internal.AST.MergeSet
  ( concatTraverse,
  )
import Data.Morpheus.Types.Internal.Validation
  ( FragmentValidator,
    SelectionValidator,
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
  ( validateUnionSelection,
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

type TypeDef = (TypeDefinition OUT VALID, FieldsDefinition OUT VALID)

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
      selection <- validateSelectionSet typeDef operationSelection
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

possibleTypes :: TypeDefinition a s -> [TypeName]
possibleTypes TypeDefinition {typeName, typeContent = DataObject {objectImplements}} = typeName : objectImplements
possibleTypes TypeDefinition {typeName} = [typeName]

vaidateFragmentSelection :: (ResolveFragment s) => Fragment RAW -> FragmentValidator s (SelectionSet VALID)
vaidateFragmentSelection f@Fragment {fragmentSelection} = do
  typeDef <- selectFragmentType f
  validateSelectionSet typeDef fragmentSelection

validateSelectionSet ::
  forall s.
  (ResolveFragment s) =>
  TypeDef ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionSet VALID)
validateSelectionSet (typeDef, fieldsDef) =
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
            fieldDef <- selectKnown (Ref selectionName selectionPosition) fieldsDef
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
    validateSelection (Spread dirs ref) =
      processSelectionDirectives
        FRAGMENT_SPREAD
        dirs
        (const $ unionTagSelection <$> resolveValidFragment vaidateFragmentSelection (possibleTypes typeDef) ref)
    validateSelection
      ( InlineFragment
          fragment@Fragment
            { fragmentDirectives
            }
        ) =
        processSelectionDirectives INLINE_FRAGMENT fragmentDirectives $
          const (validate fragment)
    validate = fmap fragmentSelection . validateFragment vaidateFragmentSelection (possibleTypes typeDef)

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
  typeDef@TypeDefinition {typeContent}
  currentSelectionRef
  rawSelectionSet =
    withScope typeDef currentSelectionRef $
      __validate typeContent
    where
      __validate :: TypeContent TRUE OUT VALID -> FragmentValidator s (SelectionContent VALID)
      -- Validate UnionSelection
      __validate DataUnion {unionMembers} =
        validateUnionSelection
          validateSelectionSet
          rawSelectionSet
          unionMembers
      -- Validate Regular selection set
      __validate DataObject {objectFields} =
        SelectionSet
          <$> validateSelectionSet
            (typeDef, objectFields)
            rawSelectionSet
      -- TODO: Union Like Validation
      __validate DataInterface {interfaceFields} =
        SelectionSet
          <$> validateSelectionSet
            (typeDef, interfaceFields)
            rawSelectionSet
      __validate _ =
        failure $
          hasNoSubfields
            currentSelectionRef
            typeDef
