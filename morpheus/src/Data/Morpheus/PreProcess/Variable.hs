{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Variable
  ( resolveOperationVariables
  , resolveArgumentValue
  , allVariableReferences
  ) where

import           Data.List                              ((\\))
import qualified Data.Map                               as M (fromList, lookup)
import           Data.Morpheus.Error.Input              (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Variable           (undefinedVariable, uninitializedVariable, unknownType,
                                                         unusedVariables, variableGotInvalidValue)
import           Data.Morpheus.PreProcess.Input.Object  (validateInputValue)
import           Data.Morpheus.PreProcess.Utils         (getInputType)
import           Data.Morpheus.Schema.Internal.Types    (InputType, TypeLib)
import           Data.Morpheus.Types.Core               (EnhancedKey (..))
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.Operator     (Variable (..))
import           Data.Morpheus.Types.Query.RawSelection (RawArgument (..), RawSelection (..), RawSelectionSet)
import qualified Data.Morpheus.Types.Query.Selection    as Valid (Argument (..))
import           Data.Morpheus.Types.Types              (Variables)
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

lookupBodyValue :: Position -> Variables -> Text -> Validation JSType
lookupBodyValue position' variables' key' = lookupVariable variables' key' (uninitializedVariable position')

handleInputError :: Text -> Int -> InputValidation JSType -> Validation (Text, JSType)
handleInputError key' position' (Left error') = Left $ variableGotInvalidValue key' (inputErrorMessage error') position'
handleInputError key' _ (Right value') = pure (key', value')

lookupAndValidateValueOnBody :: TypeLib -> Variables -> (Text, Variable) -> Validation (Text, JSType)
lookupAndValidateValueOnBody typeLib root (key', Variable type' pos) = getVariableType type' pos typeLib >>= checkType
  where
    checkType _type = do
      variableValue <- lookupBodyValue pos root key'
      handleInputError key' pos $ validateInputValue False typeLib _type (key', variableValue)

resolveOperationVariables :: TypeLib -> Variables -> [EnhancedKey] -> [(Text, Variable)] -> Validation Variables
resolveOperationVariables typeLib root references' variables' = do
  checkUnusedVariable references' variables'
  M.fromList <$> mapM (lookupAndValidateValueOnBody typeLib root) variables'

varToKey :: (Text, Variable) -> EnhancedKey
varToKey (key', Variable _ position') = EnhancedKey key' position'

checkUnusedVariable :: [EnhancedKey] -> [(Text, Variable)] -> Validation ()
checkUnusedVariable references' variables' =
  case map varToKey variables' \\ references' of
    []      -> pure ()
    unused' -> Left $ unusedVariables unused'

allVariableReferences :: [RawSelectionSet] -> [EnhancedKey]
allVariableReferences = concatMap (concatMap searchReferencesIn)

referencesFromArgument :: (Text, RawArgument) -> [EnhancedKey]
referencesFromArgument (_, Argument _ _)                       = []
referencesFromArgument (_, VariableReference value' position') = [EnhancedKey value' position']

searchReferencesIn :: (Text, RawSelection) -> [EnhancedKey]
searchReferencesIn (_, RawSelectionSet rawArgs rawSelectors _) =
  concatMap referencesFromArgument rawArgs ++ concatMap searchReferencesIn rawSelectors
searchReferencesIn (_, RawField rawArgs _ _) = concatMap referencesFromArgument rawArgs
searchReferencesIn (_, Spread _ _) = []

resolveArgumentValue :: Variables -> (Text, RawArgument) -> Validation (Text, Valid.Argument)
resolveArgumentValue root (key', VariableReference variableID pos) = do
  value <- getVariable pos root variableID
  pure (key', Valid.Argument value pos)
resolveArgumentValue _ (key', Argument value pos) = pure (key', Valid.Argument value pos)
