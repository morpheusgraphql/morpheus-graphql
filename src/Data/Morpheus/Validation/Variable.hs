{-# LANGUAGE NamedFieldPuns    #-}
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
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), FragmentLib, RawArgument (..),
                                                                RawSelection (..), RawSelection' (..), RawSelectionSet,
                                                                Reference (..))
import           Data.Morpheus.Types.Internal.AST.Selection    (Argument (..))
import           Data.Morpheus.Types.Internal.Base             (EnhancedKey (..), Position)
import           Data.Morpheus.Types.Internal.Data             (DataInputType, DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation       (Validation)
import           Data.Morpheus.Types.Internal.Value            (Value (..))
import           Data.Morpheus.Types.Types                     (Variables)
import           Data.Morpheus.Validation.Input.Object         (validateInputValue)
import           Data.Morpheus.Validation.Spread               (getFragment)
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
lookupAndValidateValueOnBody typeLib variables' (key', Variable { variableType
                                                                , variablePosition
                                                                , isVariableRequired
                                                                , variableTypeWrappers
                                                                }) =
  getVariableType variableType variablePosition typeLib >>= checkType isVariableRequired
  where
    validator _type variableValue =
      handleInputError key' variablePosition $
      validateInputValue typeLib [] variableTypeWrappers _type (key', variableValue)
    checkType True _type  = lookupBodyValue variablePosition variables' key' variableType >>= validator _type
    checkType False _type = maybe (pure (key', Null)) (validator _type) (M.lookup key' variables')

varToKey :: (Text, Variable) -> EnhancedKey
varToKey (key', Variable _ _ _ position') = EnhancedKey key' position'

referencesFromArgument :: (Text, RawArgument) -> [EnhancedKey]
referencesFromArgument (_, RawArgument {})        = []
referencesFromArgument (_, VariableReference ref) = [EnhancedKey (referenceName ref) (referencePosition ref)]

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

allVariableReferences :: FragmentLib -> [RawSelectionSet] -> Validation [EnhancedKey]
allVariableReferences fragmentLib = concatMapM (concatMapM searchReferences)
  where
    searchReferences :: (Text, RawSelection) -> Validation [EnhancedKey]
    searchReferences (_, RawSelectionSet RawSelection' {rawSelectionArguments, rawSelectionRec}) =
      getArgs <$> concatMapM searchReferences rawSelectionRec
      where
        getArgs :: [EnhancedKey] -> [EnhancedKey]
        getArgs x = concatMap referencesFromArgument rawSelectionArguments <> x
    searchReferences (_, InlineFragment Fragment {fragmentSelection}) = concatMapM searchReferences fragmentSelection
    searchReferences (_, RawAlias {rawAliasSelection}) = concatMapM searchReferences [rawAliasSelection]
    searchReferences (_, RawSelectionField RawSelection' {rawSelectionArguments}) =
      return $ concatMap referencesFromArgument rawSelectionArguments
    searchReferences (_, Spread reference) =
      getFragment reference fragmentLib >>= concatMapM searchReferences . fragmentSelection

resolveArgumentValue :: Variables -> (Text, RawArgument) -> Validation (Text, Argument)
resolveArgumentValue root (key', VariableReference Reference {referenceName, referencePosition}) = do
  value <- getVariable referencePosition root referenceName
  pure (key', Argument value referencePosition)
resolveArgumentValue _ (key', RawArgument argument') = pure (key', argument')

resolveOperatorVariables :: DataTypeLib -> FragmentLib -> Variables -> RawOperator' -> Validation Variables
resolveOperatorVariables typeLib fragmentLib root operator' = do
  allVariableReferences fragmentLib [operatorSelection operator'] >>= checkUnusedVariables
  M.fromList <$> mapM (lookupAndValidateValueOnBody typeLib root) (operatorArgs operator')
  where
    checkUnusedVariables :: [EnhancedKey] -> Validation ()
    checkUnusedVariables references' =
      case map varToKey (operatorArgs operator') \\ references' of
        []      -> pure ()
        unused' -> Left $ unusedVariables (operatorName operator') unused'
