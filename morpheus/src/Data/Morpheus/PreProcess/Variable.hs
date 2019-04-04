{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Variable
  ( validateVariables
  , replaceVariable
  ) where

import qualified Data.Map                               as M (lookup)
import           Data.Morpheus.Error.Input              (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Variable           (unknownType, variableGotInvalidValue, variableIsNotDefined)
import           Data.Morpheus.PreProcess.Input.Object  (validateInput)
import           Data.Morpheus.PreProcess.Utils         (getInputType)
import           Data.Morpheus.Schema.Internal.Types    (InputType, TypeLib)
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (MetaInfo (..), Position)
import           Data.Morpheus.Types.Query.RawSelection (RawArgument (..))
import qualified Data.Morpheus.Types.Query.Selection    as Valid (Argument (..))
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

-- asGQLError :: MetaValidation a -> Validation a
-- asGQLError (Left err)    = Left $ variableValidationError err
-- asGQLError (Right value) = pure value
getVariableType :: Text -> Position -> TypeLib -> Validation InputType
getVariableType type' position' lib' = getInputType type' lib' error'
  where
    error' = unknownType type' position'

getVariable :: Position -> GQLQueryRoot -> Text -> Validation JSType
getVariable pos root variableID =
  case M.lookup variableID (inputVariables root) of
    Nothing    -> Left $ variableIsNotDefined $ MetaInfo {typeName = "", key = variableID, position = pos}
    Just value -> pure value

handleInputError :: Text -> Int -> InputValidation a -> Validation ()
handleInputError key' position' (Left error') = Left $ variableGotInvalidValue key' (inputErrorMessage error') position'
handleInputError _ _ _ = pure ()

checkVariableType :: TypeLib -> GQLQueryRoot -> (Text, RawArgument) -> Validation ()
checkVariableType typeLib root (key', Variable tName pos) = getVariableType tName pos typeLib >>= checkType
  where
    checkType _type = do
      variableValue <- getVariable pos root key'
      handleInputError key' pos $ validateInput typeLib _type (key', variableValue)
checkVariableType _ _ (_, Argument _ _) = pure ()

validateVariables :: TypeLib -> GQLQueryRoot -> [(Text, RawArgument)] -> Validation ()
validateVariables typeLib root = mapM_ (checkVariableType typeLib root)

replaceVariable :: GQLQueryRoot -> (Text, RawArgument) -> Validation (Text, Valid.Argument)
replaceVariable root (vKey, Variable variableID pos) = do
  value <- getVariable pos root variableID
  pure (vKey, Valid.Argument value pos)
replaceVariable _ (vKey, Argument value pos) = pure (vKey, Valid.Argument value pos)
