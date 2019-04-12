{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Variable
  ( validateDefinedVariables
  , resolveArgumentValue
  , allVariableReferences
  ) where

import           Data.List                              ((\\))
import qualified Data.Map                               as M (lookup)
import           Data.Morpheus.Error.Input              (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Variable           (undefinedVariable, uninitializedVariable, unknownType,
                                                         unusedVariables, variableGotInvalidValue)
import           Data.Morpheus.PreProcess.Input.Object  (validateInput)
import           Data.Morpheus.PreProcess.Utils         (getInputType)
import           Data.Morpheus.Schema.Internal.Types    (InputType, TypeLib)
import           Data.Morpheus.Types.Core               (EnhancedKey (..))
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.Operator     (Variable (..))
import           Data.Morpheus.Types.Query.RawSelection (RawArgument (..), RawSelection (..), RawSelectionSet)
import qualified Data.Morpheus.Types.Query.Selection    as Valid (Argument (..))
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

getVariableType :: Text -> Position -> TypeLib -> Validation InputType
getVariableType type' position' lib' = getInputType type' lib' error'
  where
    error' = unknownType type' position'

lookupVariable :: GQLQueryRoot -> Text -> (Text -> error) -> Either error JSType
lookupVariable root key' error' =
  case M.lookup key' (inputVariables root) of
    Nothing    -> Left $ error' key'
    Just value -> pure value

getVariable :: Position -> GQLQueryRoot -> Text -> Validation JSType
getVariable position' root key' = lookupVariable root key' (undefinedVariable "Query" position')

lookupBodyValue :: Position -> GQLQueryRoot -> Text -> Validation JSType
lookupBodyValue position' root key' = lookupVariable root key' (uninitializedVariable position')

handleInputError :: Text -> Int -> InputValidation a -> Validation ()
handleInputError key' position' (Left error') = Left $ variableGotInvalidValue key' (inputErrorMessage error') position'
handleInputError _ _ _ = pure ()

lookupAndValidateValueOnBody :: TypeLib -> GQLQueryRoot -> (Text, Variable) -> Validation ()
lookupAndValidateValueOnBody typeLib root (key', Variable type' pos) = getVariableType type' pos typeLib >>= checkType
  where
    checkType _type = do
      variableValue <- lookupBodyValue pos root key'
      handleInputError key' pos $ validateInput typeLib _type (key', variableValue)

validateDefinedVariables :: TypeLib -> GQLQueryRoot -> [EnhancedKey] -> [(Text, Variable)] -> Validation ()
validateDefinedVariables typeLib root references' variables' =
  mapM_ (lookupAndValidateValueOnBody typeLib root) variables' >> checkUnusedVariable references' variables'

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

resolveArgumentValue :: GQLQueryRoot -> (Text, RawArgument) -> Validation (Text, Valid.Argument)
resolveArgumentValue root (key', VariableReference variableID pos) = do
  value <- getVariable pos root variableID
  pure (key', Valid.Argument value pos)
resolveArgumentValue _ (key', Argument value pos) = pure (key', Valid.Argument value pos)
