{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Variable
  ( resolveOperatorVariables
  , resolveArgumentValue
  ) where

import           Data.List                              ((\\))
import qualified Data.Map                               as M (fromList, lookup)
import           Data.Morpheus.Error.Input              (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Variable           (undefinedVariable, uninitializedVariable, unknownType,
                                                         unusedVariables, variableGotInvalidValue)
import           Data.Morpheus.Schema.Internal.AST      (InputType, TypeLib)
import           Data.Morpheus.Types.Core               (EnhancedKey (..))
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.Operator     (Operator' (..), RawOperator', Variable (..))
import           Data.Morpheus.Types.Query.RawSelection (RawArgument (..), RawSelection (..), RawSelectionSet)
import qualified Data.Morpheus.Types.Query.Selection    as Valid (Argument (..))
import           Data.Morpheus.Types.Types              (Variables)
import           Data.Morpheus.Validation.Input.Object  (validateInputValue)
import           Data.Morpheus.Validation.Utils.Utils   (getInputType)
import           Data.Text                              (Text)

getVariableType :: Text -> Position -> TypeLib -> Validation InputType
getVariableType type' position' lib' = getInputType type' lib' error'
  where
    error' = unknownType type' position'

lookupVariable :: Variables -> Text -> (Text -> error) -> Either error JSType
lookupVariable variables' key' error' =
  case M.lookup key' variables' of
    Nothing    -> Left $ error' key'
    Just value -> pure value

getVariable :: Position -> Variables -> Text -> Validation JSType
getVariable position' variables' key' = lookupVariable variables' key' (undefinedVariable "Query" position')

handleInputError :: Text -> Int -> InputValidation JSType -> Validation (Text, JSType)
handleInputError key' position' (Left error') = Left $ variableGotInvalidValue key' (inputErrorMessage error') position'
handleInputError key' _ (Right value') = pure (key', value')

lookupBodyValue :: Position -> Variables -> Text -> Text -> Validation JSType
lookupBodyValue position' variables' key' type' = lookupVariable variables' key' (uninitializedVariable position' type')

lookupNullableValue :: Variables -> Text -> Validation (Text, JSType)
lookupNullableValue variables' key' =
  case M.lookup key' variables' of
    Nothing    -> pure (key', JSNull)
    Just value -> pure (key', value)

lookupAndValidateValueOnBody :: TypeLib -> Variables -> (Text, Variable) -> Validation (Text, JSType)
lookupAndValidateValueOnBody typeLib variables' (key', Variable { variableType = type'
                                                                , variablePosition = position'
                                                                , isVariableRequired = isRequired'
                                                                }) =
  if isRequired'
    then getVariableType type' position' typeLib >>= checkType
    else lookupNullableValue variables' key'
  where
    checkType _type = do
      variableValue <- lookupBodyValue position' variables' key' type'
      handleInputError key' position' $ validateInputValue False typeLib _type (key', variableValue)

varToKey :: (Text, Variable) -> EnhancedKey
varToKey (key', Variable _ _ _ position') = EnhancedKey key' position'

checkUnusedVariable :: [EnhancedKey] -> RawOperator' -> Validation ()
checkUnusedVariable references' operator' =
  case map varToKey (operatorArgs operator') \\ references' of
    []      -> pure ()
    unused' -> Left $ unusedVariables (operatorName operator') unused'

allVariableReferences :: [RawSelectionSet] -> [EnhancedKey]
allVariableReferences = concatMap (concatMap searchReferencesIn)

referencesFromArgument :: (Text, RawArgument) -> [EnhancedKey]
referencesFromArgument (_, Argument _ _)                       = []
referencesFromArgument (_, VariableReference value' position') = [EnhancedKey value' position']

searchReferencesIn :: (Text, RawSelection) -> [EnhancedKey]
searchReferencesIn (_, RawSelectionSet rawArgs rawSelectors _) =
  concatMap referencesFromArgument rawArgs ++ concatMap searchReferencesIn rawSelectors
searchReferencesIn (_, InlineFragment _ rawSelector' _) = concatMap searchReferencesIn rawSelector'
searchReferencesIn (_, RawField rawArgs _ _) = concatMap referencesFromArgument rawArgs
searchReferencesIn (_, Spread _ _) = []

resolveArgumentValue :: Variables -> (Text, RawArgument) -> Validation (Text, Valid.Argument)
resolveArgumentValue root (key', VariableReference variableID pos) = do
  value <- getVariable pos root variableID
  pure (key', Valid.Argument value pos)
resolveArgumentValue _ (key', Argument value pos) = pure (key', Valid.Argument value pos)

resolveOperatorVariables :: TypeLib -> Variables -> RawOperator' -> Validation Variables
resolveOperatorVariables typeLib root operator' = do
  checkUnusedVariable (allVariableReferences [operatorSelection operator']) operator'
  M.fromList <$> mapM (lookupAndValidateValueOnBody typeLib root) (operatorArgs operator')
