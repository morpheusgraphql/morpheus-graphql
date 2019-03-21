{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Variable
  ( validateVariables
  , replaceVariable
  ) where

import qualified Data.Map                               as M
import           Data.Morpheus.Error.Arguments          (unsupportedArgumentType)
import           Data.Morpheus.Error.Variable           (variableIsNotDefined,
                                                         variableValidationError)
import           Data.Morpheus.PreProcess.Input.Object  (validateInputVariable)
import           Data.Morpheus.PreProcess.Utils         (existsType)
import qualified Data.Morpheus.Schema.Type              as T (kind)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (TypeLib)
import           Data.Morpheus.Types.Describer          (EnumOf (..))
import           Data.Morpheus.Types.Error              (MetaValidation, Validation)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (MetaInfo (..))
import           Data.Morpheus.Types.Query.RawSelection (RawArgument (..))
import qualified Data.Morpheus.Types.Query.Selection    as Valid (Argument (..))
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

asGQLError :: MetaValidation a -> Validation a
asGQLError (Left err)    = Left $ variableValidationError err
asGQLError (Right value) = pure value

getVariable :: Int -> GQLQueryRoot -> Text -> Validation JSType
getVariable pos root variableID =
  case M.lookup variableID (inputVariables root) of
    Nothing    -> Left $ variableIsNotDefined meta
    Just value -> pure value
  where
    meta = MetaInfo {typeName = "TODO: Name", key = variableID, position = pos}

checkVariableType :: TypeLib -> GQLQueryRoot -> (Text, RawArgument) -> Validation ()
checkVariableType typeLib root (variableID, Variable tName pos) =
  asGQLError (existsType (pos, tName) tName typeLib) >>= checkType
  where
    checkType _type =
      case T.kind _type of
        EnumOf SCALAR       -> checkTypeInp _type variableID
        EnumOf INPUT_OBJECT -> checkTypeInp _type variableID
        _                   -> Left $ unsupportedArgumentType meta
    meta = MetaInfo {typeName = tName, position = pos, key = variableID}
    checkTypeInp _type inputKey = do
      variableValue <- getVariable pos root inputKey
      _ <- asGQLError (validateInputVariable typeLib _type (inputKey, variableValue))
      pure ()
checkVariableType _ _ (_, Argument _ _) = pure ()

validateVariables :: TypeLib -> GQLQueryRoot -> [(Text, RawArgument)] -> Validation ()
validateVariables typeLib root = mapM_ (checkVariableType typeLib root)

replaceVariable :: GQLQueryRoot -> (Text, RawArgument) -> Validation (Text, Valid.Argument)
replaceVariable root (vKey, Variable variableID pos) = do
  value <- getVariable pos root variableID
  pure (vKey, Valid.Argument value pos)
replaceVariable _ (vKey, Argument value pos) = pure (vKey, Valid.Argument value pos)
