{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Variable
  ( validateDefinedVariables
  , replaceVariable
  ) where

import qualified Data.Map                               as M (lookup)
import           Data.Morpheus.Error.Input              (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Variable           (undefinedVariable, uninitializedVariable, unknownType,
                                                         variableGotInvalidValue)
import           Data.Morpheus.PreProcess.Input.Object  (validateInput)
import           Data.Morpheus.PreProcess.Utils         (getInputType)
import           Data.Morpheus.Schema.Internal.Types    (InputType, TypeLib)
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.RawSelection (RawArgument (..))
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

lookupAndValidateValueOnBody :: TypeLib -> GQLQueryRoot -> (Text, RawArgument) -> Validation ()
lookupAndValidateValueOnBody typeLib root (key', Variable tName pos) = getVariableType tName pos typeLib >>= checkType
  where
    checkType _type = do
      variableValue <- lookupBodyValue pos root key'
      handleInputError key' pos $ validateInput typeLib _type (key', variableValue)
lookupAndValidateValueOnBody _ _ (_, Argument _ _) = pure ()

validateDefinedVariables :: TypeLib -> GQLQueryRoot -> [(Text, RawArgument)] -> Validation ()
validateDefinedVariables typeLib root = mapM_ (lookupAndValidateValueOnBody typeLib root)

replaceVariable :: GQLQueryRoot -> (Text, RawArgument) -> Validation (Text, Valid.Argument)
replaceVariable root (vKey, Variable variableID pos) = do
  value <- getVariable pos root variableID
  pure (vKey, Valid.Argument value pos)
replaceVariable _ (vKey, Argument value pos) = pure (vKey, Valid.Argument value pos)
