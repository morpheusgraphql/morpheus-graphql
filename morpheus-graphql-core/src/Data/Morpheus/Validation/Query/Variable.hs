{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Query.Variable
  ( resolveOperationVariables,
  )
where

import qualified Data.HashMap.Lazy as M
  ( lookup,
  )
import Data.Maybe (maybe)
--- MORPHEUS
import Data.Morpheus.Error.Variable (uninitializedVariable)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    DefaultValue,
    Directive (..),
    Fragment (..),
    IN,
    ObjectEntry (..),
    Operation (..),
    RAW,
    RawValue,
    Ref (..),
    ResolvedValue,
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeDefinition,
    TypeNameRef (..),
    TypeRef (..),
    VALID,
    VALIDATION_MODE (..),
    ValidValue,
    Value (..),
    Variable (..),
    VariableContent (..),
    VariableDefinitions,
    Variables,
    isNullable,
  )
import Data.Morpheus.Types.Internal.Validation
  ( BaseValidator,
    Constraint (..),
    InputSource (..),
    askFragments,
    askSchema,
    checkUnused,
    constraint,
    selectKnown,
    startInput,
    withPosition,
  )
import Data.Morpheus.Validation.Internal.Value
  ( validateInput,
  )
import Data.Semigroup ((<>))

class ExploreRefs a where
  exploreRefs :: a -> [Ref]

instance ExploreRefs RawValue where
  exploreRefs (VariableValue ref) = [ref]
  exploreRefs (Object fields) = concatMap (exploreRefs . entryValue) fields
  exploreRefs (List ls) = concatMap exploreRefs ls
  exploreRefs _ = []

instance ExploreRefs (Directive RAW) where
  exploreRefs Directive {directiveArgs} = concatMap exploreRefs directiveArgs

instance ExploreRefs (Argument RAW) where
  exploreRefs = exploreRefs . argumentValue

mapSelection :: (Selection RAW -> BaseValidator [b]) -> SelectionSet RAW -> BaseValidator [b]
mapSelection f = fmap concat . traverse f

allVariableRefs :: [SelectionSet RAW] -> BaseValidator [Ref]
allVariableRefs = fmap concat . traverse (mapSelection searchRefs)
  where
    exploreSelectionContent :: SelectionContent RAW -> BaseValidator [Ref]
    exploreSelectionContent SelectionField = pure []
    exploreSelectionContent (SelectionSet selSet) = mapSelection searchRefs selSet
    ---------------------------------------
    searchRefs :: Selection RAW -> BaseValidator [Ref]
    searchRefs Selection {selectionArguments, selectionDirectives, selectionContent} = do
      let directiveRefs = concatMap exploreRefs selectionDirectives
      contentRefs <- exploreSelectionContent selectionContent
      pure $ directiveRefs <> contentRefs <> concatMap exploreRefs selectionArguments
    searchRefs (InlineFragment Fragment {fragmentSelection, fragmentDirectives}) =
      (concatMap exploreRefs fragmentDirectives <>)
        <$> mapSelection searchRefs fragmentSelection
    searchRefs (Spread directives reference) =
      (concatMap exploreRefs directives <>)
        <$> ( askFragments
                >>= selectKnown reference
                >>= mapSelection searchRefs
                . fragmentSelection
            )

resolveOperationVariables ::
  Variables ->
  VALIDATION_MODE ->
  Operation RAW ->
  BaseValidator (VariableDefinitions VALID)
resolveOperationVariables
  root
  validationMode
  Operation
    { operationSelection,
      operationArguments
    } =
    checkUnusedVariables
      *> traverse (lookupAndValidateValueOnBody root validationMode) operationArguments
    where
      checkUnusedVariables :: BaseValidator ()
      checkUnusedVariables = do
        uses <- allVariableRefs [operationSelection]
        checkUnused uses (elems operationArguments)

lookupAndValidateValueOnBody ::
  Variables ->
  VALIDATION_MODE ->
  Variable RAW ->
  BaseValidator (Variable VALID)
lookupAndValidateValueOnBody
  bodyVariables
  validationMode
  var@Variable
    { variableName,
      variableType,
      variablePosition,
      variableValue = DefaultValue defaultValue
    } =
    withPosition variablePosition $
      toVariable
        <$> ( askSchema
                >>= selectKnown (TypeNameRef (typeConName variableType) variablePosition)
                >>= constraint INPUT var
                >>= checkType getVariable defaultValue
            )
    where
      toVariable x = var {variableValue = ValidVariableValue x}
      getVariable :: Maybe ResolvedValue
      getVariable = M.lookup variableName bodyVariables
      ------------------------------------------------------------------
      -- checkType ::
      checkType ::
        Maybe ResolvedValue ->
        DefaultValue ->
        TypeDefinition IN ->
        BaseValidator ValidValue
      checkType (Just variable) Nothing varType = validator varType False variable
      checkType (Just variable) (Just defValue) varType =
        validator varType True defValue >> validator varType False variable
      checkType Nothing (Just defValue) varType = validator varType True defValue
      checkType Nothing Nothing varType
        | validationMode /= WITHOUT_VARIABLES && not (isNullable variableType) =
          failure $ uninitializedVariable var
        | otherwise =
          returnNull
        where
          returnNull =
            maybe (pure Null) (validator varType False) (M.lookup variableName bodyVariables)
      -----------------------------------------------------------------------------------------------
      validator :: TypeDefinition IN -> Bool -> ResolvedValue -> BaseValidator ValidValue
      validator varType isDefaultValue varValue =
        startInput (SourceVariable var isDefaultValue) $
          validateInput
            (typeWrappers variableType)
            varType
            (ObjectEntry variableName varValue)
