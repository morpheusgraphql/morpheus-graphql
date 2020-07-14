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
  )
where

import Control.Applicative ((<*>), pure)
import Control.Monad ((>>=), fmap)
import Data.Foldable (null)
import Data.Functor (($>), (<$>))
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
    FieldName,
    FieldsDefinition,
    Fragment (..),
    GQLError (..),
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
    VALID,
    isEntNode,
    msg,
  )
import Data.Morpheus.Types.Internal.AST.MergeSet
  ( concatTraverse,
  )
import Data.Morpheus.Types.Internal.Validation
  ( SelectionValidator,
    askFieldType,
    getOperationObjectType,
    selectKnown,
    withScope,
  )
import Data.Morpheus.Validation.Internal.Arguments
  ( validateFieldArguments,
  )
import Data.Morpheus.Validation.Internal.Directive
  ( shouldIncludeSelection,
    validateQueryDirectives,
  )
import Data.Morpheus.Validation.Query.Fragment
  ( castFragmentType,
    resolveSpread,
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

singleTopLevelSelectionError :: Maybe FieldName -> Selection VALID -> GQLError
singleTopLevelSelectionError name Selection {selectionPosition} =
  GQLError
    { message =
        subscriptionName
          <> " must select "
          <> "only one top level field.",
      locations = [selectionPosition]
    }
  where
    subscriptionName = maybe "Anonymous Subscription" (("Subscription " <>) . msg) name

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
      typeDef <- getOperationObjectType rawOperation
      selection <- validateSelectionSet typeDef operationSelection
      singleTopLevelSelection rawOperation selection
      directives <-
        validateQueryDirectives
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
  (Directives VALID -> SelectionValidator (SelectionSet VALID)) ->
  SelectionValidator (SelectionSet VALID)
processSelectionDirectives location rawDirectives sel = do
  directives <- validateQueryDirectives location rawDirectives
  include <- shouldIncludeSelection directives
  selection <- sel directives
  pure $
    if include
      then selection
      else empty

validateSelectionSet ::
  TypeDef -> SelectionSet RAW -> SelectionValidator (SelectionSet VALID)
validateSelectionSet dataType@(typeDef, fieldsDef) =
  concatTraverse validateSelection
  where
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
    validateSelection :: Selection RAW -> SelectionValidator (SelectionSet VALID)
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
          commonValidation :: SelectionValidator (TypeDefinition OUT VALID, Arguments VALID)
          commonValidation = do
            fieldDef <- selectKnown (Ref selectionName selectionPosition) fieldsDef
            (,)
              <$> askFieldType fieldDef
              <*> validateFieldArguments fieldDef selectionArguments
          -----------------------------------------------------------------------------------
          validateSelectionContent :: Directives VALID -> SelectionContent RAW -> SelectionValidator (SelectionSet VALID)
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
      processSelectionDirectives FRAGMENT_SPREAD dirs
        $ const
        -- TODO: add directives to selection
        $ resolveSpread [typeName typeDef] ref
          >>= validateFragment
    validateSelection
      ( InlineFragment
          fragment@Fragment
            { fragmentDirectives,
              fragmentPosition
            }
        ) =
        processSelectionDirectives INLINE_FRAGMENT fragmentDirectives
          $ const
          -- TODO: add directives to selection
          $ castFragmentType Nothing fragmentPosition [typeName typeDef] fragment
            >>= validateFragment
    --------------------------------------------------------------------------------
    validateFragment Fragment {fragmentSelection} = validateSelectionSet dataType fragmentSelection

validateContentLeaf ::
  Ref ->
  TypeDefinition OUT VALID ->
  SelectionValidator (SelectionContent s)
validateContentLeaf
  (Ref selectionName selectionPosition)
  TypeDefinition {typeName, typeContent}
    | isEntNode typeContent = pure SelectionField
    | otherwise =
      failure $ subfieldsNotSelected selectionName typeName selectionPosition

validateByTypeContent ::
  TypeDefinition OUT VALID ->
  Ref ->
  SelectionSet RAW ->
  SelectionValidator (SelectionContent VALID)
validateByTypeContent
  typeDef@TypeDefinition {typeContent}
  currentSelectionRef
  rawSelectionSet =
    withScope typeDef currentSelectionRef $
      __validate typeContent
    where
      __validate :: TypeContent TRUE OUT VALID -> SelectionValidator (SelectionContent VALID)
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
