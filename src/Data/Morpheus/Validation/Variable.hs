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
import           Data.Morpheus.Types.Internal.AST.RawSelection (RawArgument (..), RawSelection (..), RawSelectionSet)
import qualified Data.Morpheus.Types.Internal.AST.Selection    as Valid (Argument (..))
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

handleInputError :: Text -> Int -> InputValidation Value -> Validation (Text, Value)
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

resolveOperatorVariables :: DataTypeLib -> Variables -> RawOperator' -> Validation Variables
resolveOperatorVariables typeLib root operator' = do
  checkUnusedVariable (allVariableReferences [operatorSelection operator']) operator'
  M.fromList <$> mapM (lookupAndValidateValueOnBody typeLib root) (operatorArgs operator')
