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
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    DefaultValue,
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
import Data.Morpheus.Types.Internal.Operation
  ( Failure (..),
    Listable (..),
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
    withScopePosition,
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

instance ExploreRefs (Argument RAW) where
  exploreRefs = exploreRefs . argumentValue

mapSelection :: (Selection RAW -> BaseValidator [b]) -> SelectionSet RAW -> BaseValidator [b]
mapSelection f = fmap concat . traverse f

allVariableRefs :: [SelectionSet RAW] -> BaseValidator [Ref]
allVariableRefs = fmap concat . traverse (mapSelection searchRefs)
  where
    searchRefs :: Selection RAW -> BaseValidator [Ref]
    searchRefs Selection {selectionArguments, selectionContent = SelectionField} =
      return $ concatMap exploreRefs selectionArguments
    searchRefs Selection {selectionArguments, selectionContent = SelectionSet selSet} =
      getArgs <$> mapSelection searchRefs selSet
      where
        getArgs :: [Ref] -> [Ref]
        getArgs x = concatMap exploreRefs selectionArguments <> x
    searchRefs (InlineFragment Fragment {fragmentSelection}) =
      mapSelection searchRefs fragmentSelection
    searchRefs (Spread reference) =
      askFragments
        >>= selectKnown reference
        >>= mapSelection searchRefs
        . fragmentSelection

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
        checkUnused uses (toList operationArguments)

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
    withScopePosition variablePosition $
      toVariable
        <$> ( askSchema
                >>= selectKnown (Ref (typeConName variableType) variablePosition)
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
      checkType (Just variable) Nothing varType = validator varType variable
      checkType (Just variable) (Just defValue) varType =
        validator varType defValue >> validator varType variable
      checkType Nothing (Just defValue) varType = validator varType defValue
      checkType Nothing Nothing varType
        | validationMode /= WITHOUT_VARIABLES && not (isNullable variableType) =
          failure $ uninitializedVariable var
        | otherwise =
          returnNull
        where
          returnNull =
            maybe (pure Null) (validator varType) (M.lookup variableName bodyVariables)
      -----------------------------------------------------------------------------------------------
      validator :: TypeDefinition IN -> ResolvedValue -> BaseValidator ValidValue
      validator varType varValue =
        startInput (SourceVariable var) $
          validateInput
            (typeWrappers variableType)
            varType
            (ObjectEntry variableName varValue)
