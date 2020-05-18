{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Validation.Query.Selection
  ( validateOperation,
  )
where

-- MORPHEUS
import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Error.Selection
  ( hasNoSubfields,
    subfieldsNotSelected,
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
    empty,
    keyOf,
    selectBy,
    selectOr,
    singleton,
  )
import Data.Morpheus.Schema.Directives (defaultDirectives)
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    Directive (..),
    DirectiveDefinition (..),
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
    ScalarValue (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    Value (..),
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
import Data.Morpheus.Validation.Query.Arguments
  ( validateDirectiveArguments,
    validateFieldArguments,
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
      pure $
        Operation
          { operationName,
            operationType,
            operationArguments = empty,
            operationSelection = selection
          }

validateDirective :: [DirectiveDefinition] -> Directive RAW -> SelectionValidator (Directive VALID)
validateDirective directiveDefs Directive {directiveName, directiveArgs} = do
  directiveDef <- selectKnown directiveName directiveDefs
  args <- validateDirectiveArguments directiveDef directiveArgs
  pure Directive {directiveName, directiveArgs = args}

validateDirectives :: Directives RAW -> SelectionValidator (Bool, Directives VALID)
validateDirectives rawDirectives = do
  directives <- traverse (validateDirective defaultDirectives) rawDirectives
  dontSkip <- directiveFulfilled False "skip" directives
  include <- directiveFulfilled True "include" directives
  pure (dontSkip && include, directives)

directiveFulfilled :: Bool -> FieldName -> Directives s -> SelectionValidator Bool
directiveFulfilled target = selectOr (pure True) (argumentIf target)

argumentIf :: Bool -> Directive s -> SelectionValidator Bool
argumentIf target Directive {directiveName, directiveArgs} =
  selectBy err "if'" directiveArgs
    >>= assertArgument target
  where
    err = globalErrorMessage $ "Directive " <> msg ("@" <> directiveName) <> " argument \"if\" of type \"Boolean!\" is required but not provided."

assertArgument :: Bool -> Argument s -> SelectionValidator Bool
assertArgument asserted Argument {argumentValue = Scalar (Boolean actual)} = pure (asserted == actual)
assertArgument _ Argument {argumentValue} = failure $ "Expected type Boolean!, found " <> msg argumentValue <> "."

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
          $ do
            (include, directives) <- validateDirectives selectionDirectives
            selection <- validateSelectionContent directives selectionContent
            pure $
              if include
                then selection
                else empty
        where
          currentSelectionRef = Ref selectionName selectionPosition
          commonValidation :: SelectionValidator (TypeDefinition OUT, Arguments VALID)
          commonValidation = do
            (fieldDef :: FieldDefinition OUT) <- selectKnown (Ref selectionName selectionPosition) fieldsDef
            -- validate field Argument -----
            arguments <-
              validateArguments
                (fieldArgs fieldDef)
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
    validateSelection (Spread _ ref) =
      resolveSpread [typeName] ref
        >>= validateFragment
    validateSelection (InlineFragment fragment') =
      castFragmentType Nothing (fragmentPosition fragment') [typeName] fragment'
        >>= validateFragment
    --------------------------------------------------------------------------------
    validateFragment Fragment {fragmentSelection} = validateSelectionSet dataType fragmentSelection
