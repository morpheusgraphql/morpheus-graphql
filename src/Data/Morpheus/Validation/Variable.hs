{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Variable
  ( resolveOperatorVariables
  , resolveArgumentValue
  ) where

import           Data.List                                     ((\\))
import qualified Data.Map                                      as M (fromList, lookup)
import           Data.Maybe                                    (maybe)
import           Data.Morpheus.Error.Input                     (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Variable                  (undefinedVariable, uninitializedVariable, unknownType,
                                                                unusedVariables, variableGotInvalidValue)
import           Data.Morpheus.Types.Internal.AST.Operator     (Operator' (..), RawOperator', Variable (..))
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), RawArgument (..), RawSelection (..),
                                                                RawSelection' (..), RawSelectionSet, Reference (..))
import           Data.Morpheus.Types.Internal.AST.Selection    (Argument (..))
import           Data.Morpheus.Types.Internal.Base             (EnhancedKey (..), Position)
import           Data.Morpheus.Types.Internal.Data             (DataInputType, DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation       (Validation)
import           Data.Morpheus.Types.Internal.Value            (Value (..))
import           Data.Morpheus.Types.Types                     (Variables)
import           Data.Morpheus.Validation.Input.Object         (validateInputValue)
import           Data.Morpheus.Validation.Utils.Utils          (getInputType)
import           Data.Text                                     (Text)

getVariableType :: Text -> Position -> DataTypeLib -> Validation DataInputType
getVariableType type' position' lib' = getInputType type' lib' error'
  where
    error' = unknownType type' position'

lookupVariable :: Variables -> Text -> (Text -> error) -> Either error Value
lookupVariable variables' key' error' =
  case M.lookup key' variables' of
    Nothing    -> Left $ error' key'
    Just value -> pure value

getVariable :: Position -> Variables -> Text -> Validation Value
getVariable position' variables' key' = lookupVariable variables' key' (undefinedVariable "Query" position')

handleInputError :: Text -> Position -> InputValidation Value -> Validation (Text, Value)
handleInputError key' position' (Left error') = Left $ variableGotInvalidValue key' (inputErrorMessage error') position'
handleInputError key' _ (Right value') = pure (key', value')

lookupBodyValue :: Position -> Variables -> Text -> Text -> Validation Value
lookupBodyValue position' variables' key' type' = lookupVariable variables' key' (uninitializedVariable position' type')

lookupAndValidateValueOnBody :: DataTypeLib -> Variables -> (Text, Variable) -> Validation (Text, Value)
lookupAndValidateValueOnBody typeLib variables' (key', Variable { variableType = type'
                                                                , variablePosition = position'
                                                                , isVariableRequired = isRequired'
                                                                , variableTypeWrappers = wrappers'
                                                                }) =
  getVariableType type' position' typeLib >>= checkType isRequired'
  where
    validator _type variableValue =
      handleInputError key' position' $ validateInputValue typeLib [] wrappers' _type (key', variableValue)
    checkType True _type  = lookupBodyValue position' variables' key' type' >>= validator _type
    checkType False _type = maybe (pure (key', Null)) (validator _type) (M.lookup key' variables')

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
referencesFromArgument (_, RawArgument {})        = []
referencesFromArgument (_, VariableReference ref) = [EnhancedKey (referenceName ref) (referencePosition ref)]

searchReferencesIn :: (Text, RawSelection) -> [EnhancedKey]
searchReferencesIn (_, RawSelectionSet RawSelection' {rawSelectionArguments = args', rawSelectionRec = selectionSet'}) =
  concatMap referencesFromArgument args' ++ concatMap searchReferencesIn selectionSet'
searchReferencesIn (_, InlineFragment Fragment {fragmentSelection = rawSelection'}) =
  concatMap searchReferencesIn rawSelection'
searchReferencesIn (_, RawAlias {rawAliasSelection = rawSelection'}) = concatMap searchReferencesIn [rawSelection']
searchReferencesIn (_, RawSelectionField RawSelection' {rawSelectionArguments = args'}) =
  concatMap referencesFromArgument args'
searchReferencesIn (_, Spread {}) = [] -- TODO: search in referenced Fragments

resolveArgumentValue :: Variables -> (Text, RawArgument) -> Validation (Text, Argument)
resolveArgumentValue root (key', VariableReference Reference {referenceName = name', referencePosition = position'}) = do
  value <- getVariable position' root name'
  pure (key', Argument value position')
resolveArgumentValue _ (key', RawArgument argument') = pure (key', argument')

resolveOperatorVariables :: DataTypeLib -> Variables -> RawOperator' -> Validation Variables
resolveOperatorVariables typeLib root operator' = do
  checkUnusedVariable (allVariableReferences [operatorSelection operator']) operator'
  M.fromList <$> mapM (lookupAndValidateValueOnBody typeLib root) (operatorArgs operator')
