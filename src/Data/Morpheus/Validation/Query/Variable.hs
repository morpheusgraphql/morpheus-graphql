{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Query.Variable
  ( resolveOperationVariables
  ) where

import           Data.List                                     ((\\))
import qualified Data.Map                                      as M (lookup)
import           Data.Maybe                                    (maybe)
import           Data.Morpheus.Error.Input                     (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Variable                  (uninitializedVariable, unknownType, unusedVariables,
                                                                variableGotInvalidValue)
import           Data.Morpheus.Types.Internal.AST.Operation    (DefaultValue, Operation (..), RawOperation,
                                                                ValidVariables, Variable (..))
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), FragmentLib, RawArgument (..),
                                                                RawSelection (..), RawSelection' (..), RawSelectionSet,
                                                                Reference (..))
import           Data.Morpheus.Types.Internal.Base             (EnhancedKey (..), Position)
import           Data.Morpheus.Types.Internal.Data             (DataKind, DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation       (Validation)
import           Data.Morpheus.Types.Internal.Value            (Value (..))
import           Data.Morpheus.Types.Types                     (Variables)
import           Data.Morpheus.Validation.Internal.Utils       (VALIDATION_MODE (..), getInputType)
import           Data.Morpheus.Validation.Internal.Value       (validateInputValue)
import           Data.Morpheus.Validation.Query.Fragment       (getFragment)
import           Data.Semigroup                                ((<>))
import           Data.Text                                     (Text)

getVariableType :: Text -> Position -> DataTypeLib -> Validation DataKind
getVariableType type' position' lib' = getInputType type' lib' error'
  where
    error' = unknownType type' position'

handleInputError :: Text -> Position -> InputValidation Value -> Validation (Text, Value)
handleInputError key' position' (Left error') = Left $ variableGotInvalidValue key' (inputErrorMessage error') position'
handleInputError key' _ (Right value') = pure (key', value')

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

allVariableReferences :: FragmentLib -> [RawSelectionSet] -> Validation [EnhancedKey]
allVariableReferences fragmentLib = concatMapM (concatMapM searchReferences)
  where
    referencesFromArgument :: (Text, RawArgument) -> [EnhancedKey]
    referencesFromArgument (_, RawArgument {}) = []
    referencesFromArgument (_, VariableReference Reference {referenceName, referencePosition}) =
      [EnhancedKey referenceName referencePosition]
    -- | search used variables in every arguments
    searchReferences :: (Text, RawSelection) -> Validation [EnhancedKey]
    searchReferences (_, RawSelectionSet RawSelection' {rawSelectionArguments, rawSelectionRec}) =
      getArgs <$> concatMapM searchReferences rawSelectionRec
      where
        getArgs :: [EnhancedKey] -> [EnhancedKey]
        getArgs x = concatMap referencesFromArgument rawSelectionArguments <> x
    searchReferences (_, InlineFragment Fragment {fragmentSelection}) = concatMapM searchReferences fragmentSelection
    searchReferences (_, RawAlias {rawAliasSelection}) = searchReferences rawAliasSelection
    searchReferences (_, RawSelectionField RawSelection' {rawSelectionArguments}) =
      return $ concatMap referencesFromArgument rawSelectionArguments
    searchReferences (_, Spread reference) =
      getFragment reference fragmentLib >>= concatMapM searchReferences . fragmentSelection

resolveOperationVariables ::
     DataTypeLib -> FragmentLib -> Variables -> VALIDATION_MODE -> RawOperation -> Validation ValidVariables
resolveOperationVariables typeLib lib root validationMode Operation {operationName, operationSelection, operationArgs} = do
  allVariableReferences lib [operationSelection] >>= checkUnusedVariables
  mapM (lookupAndValidateValueOnBody typeLib root validationMode) operationArgs
  where
    varToKey :: (Text, Variable a) -> EnhancedKey
    varToKey (key', Variable {variablePosition}) = EnhancedKey key' variablePosition
    --
    checkUnusedVariables :: [EnhancedKey] -> Validation ()
    checkUnusedVariables refs =
      case map varToKey operationArgs \\ refs of
        []      -> pure ()
        unused' -> Left $ unusedVariables operationName unused'

lookupAndValidateValueOnBody ::
     DataTypeLib -> Variables -> VALIDATION_MODE -> (Text, Variable DefaultValue) -> Validation (Text, Variable Value)
lookupAndValidateValueOnBody typeLib bodyVariables validationMode (key, var@Variable { variableType
                                                                                     , variablePosition
                                                                                     , isVariableRequired
                                                                                     , variableTypeWrappers
                                                                                     , variableValue = defaultValue
                                                                                     }) =
  toVariable <$> (getVariableType variableType variablePosition typeLib >>= checkType getVariable defaultValue)
  where
    toVariable (varKey, variableValue) = (varKey, var {variableValue})
    getVariable = M.lookup key bodyVariables
    ------------------------------------------------------------------
    checkType (Just variable) Nothing varType = validator varType variable
    checkType (Just variable) (Just defValue) varType = validator varType defValue >> validator varType variable
    checkType Nothing (Just defValue) varType = validator varType defValue
    checkType Nothing Nothing varType
      | validationMode /= WITHOUT_VARIABLES && isVariableRequired =
        Left $ uninitializedVariable variablePosition variableType key
      | otherwise = returnNull
      where
        returnNull = maybe (pure (key, Null)) (validator varType) (M.lookup key bodyVariables)
    -----------------------------------------------------------------------------------------------
    validator varType varValue =
      handleInputError key variablePosition $ validateInputValue typeLib [] variableTypeWrappers varType (key, varValue)
