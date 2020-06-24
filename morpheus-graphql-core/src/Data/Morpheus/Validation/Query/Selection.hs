{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Validation.Query.Selection
  ( validateOperation,
  )
where

-- MORPHEUS
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
    FieldDefinition,
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
    TypeName,
    VALID,
    getOperationDataType,
    isEntNode,
    msg,
  )
import Data.Morpheus.Types.Internal.AST.MergeSet
  ( concatTraverse,
  )
import Data.Morpheus.Types.Internal.Validation
  ( SelectionValidator,
    askFieldType,
    askSchema,
    selectKnown,
    withScope,
  )
import Data.Morpheus.Validation.Internal.Directive
  ( shouldIncludeSelection,
    validateDirectives,
  )
import Data.Morpheus.Validation.Query.Arguments
  ( validateFieldArguments,
  )
import Data.Morpheus.Validation.Query.Fragment
  ( castFragmentType,
    resolveSpread,
  )
import Data.Morpheus.Validation.Query.UnionSelection
  ( validateUnionSelection,
  )
import Data.Semigroup ((<>))

type TypeDef = (TypeName, FieldsDefinition OUT)

getOperationObject ::
  Operation a -> SelectionValidator (TypeName, FieldsDefinition OUT)
getOperationObject operation = do
  dt <- askSchema >>= getOperationDataType operation
  case dt of
    TypeDefinition {typeContent = DataObject {objectFields}, typeName} -> pure (typeName, objectFields)
    TypeDefinition {typeName} ->
      failure $
        "Type Mismatch: operation \""
          <> msg typeName
          <> "\" must be an Object"

selectionsWitoutTypename :: SelectionSet VALID -> [Selection VALID]
selectionsWitoutTypename = filter (("__typename" /=) . keyOf) . elems

singleTopLevelSelection :: Operation RAW -> SelectionSet VALID -> SelectionValidator ()
singleTopLevelSelection Operation {operationType = Subscription, operationName} selSet =
  case selectionsWitoutTypename selSet of
    (_ : xs) | not (null xs) -> failure $ map (singleTopLevelSelectionError operationName) xs
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
      typeDef <- getOperationObject rawOperation
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
  (Directives VALID -> SelectionValidator (SelectionSet VALID)) ->
  SelectionValidator (SelectionSet VALID)
processSelectionDirectives location rawDirectives sel = do
  directives <- validateDirectives location rawDirectives
  include <- shouldIncludeSelection directives
  selection <- sel directives
  pure $
    if include
      then selection
      else empty

validateSelectionSet ::
  TypeDef -> SelectionSet RAW -> SelectionValidator (SelectionSet VALID)
validateSelectionSet dataType@(typeName, fieldsDef) =
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
          typeName
          currentSelectionRef
          $ processSelectionDirectives
            FIELD
            selectionDirectives
            (`validateSelectionContent` selectionContent)
        where
          currentSelectionRef = Ref selectionName selectionPosition
          commonValidation :: SelectionValidator (TypeDefinition OUT, Arguments VALID)
          commonValidation = do
            (fieldDef :: FieldDefinition OUT) <- selectKnown (Ref selectionName selectionPosition) fieldsDef
            -- validate field Argument -----
            arguments <-
              validateFieldArguments
                fieldDef
                selectionArguments
            -- check field Type existence  -----
            (typeDef :: TypeDefinition OUT) <- askFieldType fieldDef
            pure (typeDef, arguments)
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
              isLeaf datatype
              pure $ singleton $
                sel
                  { selectionArguments = validArgs,
                    selectionDirectives = directives,
                    selectionContent = SelectionField
                  }
            where
              ------------------------------------------------------------
              isLeaf :: TypeDefinition OUT -> SelectionValidator ()
              isLeaf TypeDefinition {typeName = typename, typeContent}
                | isEntNode typeContent = pure ()
                | otherwise =
                  failure $
                    subfieldsNotSelected selectionName typename selectionPosition
          ----- SelectionSet
          validateSelectionContent directives (SelectionSet rawSelectionSet) =
            do
              (TypeDefinition {typeName = name, typeContent}, validArgs) <- commonValidation
              selContent <- withScope name currentSelectionRef $ validateByTypeContent name typeContent
              pure $ singleton $
                sel
                  { selectionArguments = validArgs,
                    selectionDirectives = directives,
                    selectionContent = selContent
                  }
            where
              validateByTypeContent :: TypeName -> TypeContent TRUE OUT -> SelectionValidator (SelectionContent VALID)
              -- Validate UnionSelection
              validateByTypeContent _ DataUnion {unionMembers} =
                validateUnionSelection
                  validateSelectionSet
                  rawSelectionSet
                  unionMembers
              -- Validate Regular selection set
              validateByTypeContent typename DataObject {objectFields} =
                SelectionSet
                  <$> validateSelectionSet
                    (typename, objectFields)
                    rawSelectionSet
              validateByTypeContent typename DataInterface {interfaceFields} =
                SelectionSet
                  <$> validateSelectionSet
                    (typename, interfaceFields)
                    rawSelectionSet
              validateByTypeContent typename _ =
                failure $
                  hasNoSubfields
                    (Ref selectionName selectionPosition)
                    typename
    validateSelection (Spread dirs ref) =
      processSelectionDirectives FRAGMENT_SPREAD dirs
        $ const
        -- TODO: add directives to selection
        $ resolveSpread [typeName] ref
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
          $ castFragmentType Nothing fragmentPosition [typeName] fragment
            >>= validateFragment
    --------------------------------------------------------------------------------
    validateFragment Fragment {fragmentSelection} = validateSelectionSet dataType fragmentSelection
