{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Variable
  ( resolveOperatorVariables
  ) where

import           Data.List                                     ((\\))
import qualified Data.Map                                      as M (lookup)
import           Data.Maybe                                    (maybe)
import           Data.Morpheus.Error.Input                     (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Variable                  (uninitializedVariable, unknownType, unusedVariables,
                                                                variableGotInvalidValue)
import           Data.Morpheus.Types.Internal.AST.Operator     (Operator' (..), RawOperator', ValidVariables,
                                                                Variable (..))
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), FragmentLib, RawArgument (..),
                                                                RawSelection (..), RawSelection' (..), RawSelectionSet,
                                                                Reference (..))
import           Data.Morpheus.Types.Internal.Base             (EnhancedKey (..), Position)
import           Data.Morpheus.Types.Internal.Data             (DataInputType, DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation       (Validation)
import           Data.Morpheus.Types.Internal.Value            (Value (..))
import           Data.Morpheus.Types.Types                     (Variables)
import           Data.Morpheus.Validation.Fragment             (getFragment)
import           Data.Morpheus.Validation.Input.Object         (validateInputValue)
import           Data.Morpheus.Validation.Utils.Utils          (getInputType)
import           Data.Semigroup                                ((<>))
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

resolveOperatorVariables :: DataTypeLib -> FragmentLib -> Variables -> RawOperator' -> Validation ValidVariables
resolveOperatorVariables typeLib fragmentLib root operator' = do
  allVariableReferences fragmentLib [operatorSelection operator'] >>= checkUnusedVariables
  mapM (lookupAndValidateValueOnBody typeLib root) (operatorArgs operator')
  where
    varToKey :: (Text, Variable ()) -> EnhancedKey
    varToKey (key', Variable {variablePosition}) = EnhancedKey key' variablePosition
    --
    checkUnusedVariables :: [EnhancedKey] -> Validation ()
    checkUnusedVariables references' =
      case map varToKey (operatorArgs operator') \\ references' of
        []      -> pure ()
        unused' -> Left $ unusedVariables (operatorName operator') unused'

lookupAndValidateValueOnBody :: DataTypeLib -> Variables -> (Text, Variable ()) -> Validation (Text, Variable Value)
lookupAndValidateValueOnBody typeLib bodyVariables (key', var@Variable { variableType
                                                                       , variablePosition
                                                                       , isVariableRequired
                                                                       , variableTypeWrappers
                                                                       }) =
  toVariable <$> (getVariableType variableType variablePosition typeLib >>= checkType isVariableRequired)
  where
    toVariable (k, x) = (k, var {variableValue = x})
    checkType True _type =
      lookupVariable bodyVariables key' (uninitializedVariable variablePosition variableType) >>= validator _type
    checkType False _type = maybe (pure (key', Null)) (validator _type) (M.lookup key' bodyVariables)
    validator _type varValue =
      handleInputError key' variablePosition $ validateInputValue typeLib [] variableTypeWrappers _type (key', varValue)
