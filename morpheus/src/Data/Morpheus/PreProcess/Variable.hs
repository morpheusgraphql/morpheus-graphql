{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Variable
  ( checkQueryVariables
  , replaceVariable
  ) where

import qualified Data.Map                             as M
import           Data.Morpheus.ErrorMessage           (unsupportedArgumentType,
                                                       variableIsNotDefined)
import           Data.Morpheus.PreProcess.InputObject (validateInputVariable)
import           Data.Morpheus.PreProcess.Utils       (existsType)
import qualified Data.Morpheus.Schema.GQL__Type       as T
import           Data.Morpheus.Schema.GQL__TypeKind   (GQL__TypeKind (..))
import           Data.Morpheus.Types.Introspection    (GQLTypeLib)
import           Data.Morpheus.Types.JSType           (JSType (..))
import           Data.Morpheus.Types.MetaInfo         (MetaInfo (..))
import           Data.Morpheus.Types.Types            (Argument (..), EnumOf (..),
                                                       GQLQueryRoot (..), Validation)
import           Data.Text                            (Text)

getVariable :: Int -> GQLQueryRoot -> Text -> Validation JSType
getVariable pos root key =
  case M.lookup key (inputVariables root) of
    Nothing    -> Left $ variableIsNotDefined meta
    Just value -> pure value
  where
    meta = MetaInfo {typeName = "TODO: Name", key = key, position = pos}

checkVariableType :: GQLTypeLib -> GQLQueryRoot -> (Text, Argument) -> Validation (Text, Argument)
checkVariableType typeLib root (key, Variable tName pos) = existsType tName typeLib >>= checkType
  where
    checkType _type =
      case T.kind _type of
        EnumOf SCALAR       -> checkTypeInp _type key
        EnumOf INPUT_OBJECT -> checkTypeInp _type key
        _                   -> Left $ unsupportedArgumentType meta
    meta = MetaInfo {typeName = tName, position = pos, key = key}
    checkTypeInp _type key = do
      variableValue <- getVariable pos root key
      validateInputVariable typeLib _type (key, variableValue)
      pure (key, Variable tName pos)

checkQueryVariables :: GQLTypeLib -> GQLQueryRoot -> [(Text, Argument)] -> Validation [(Text, Argument)]
checkQueryVariables typeLib root = mapM (checkVariableType typeLib root)

replaceVariable :: GQLQueryRoot -> Argument -> Validation Argument
replaceVariable root (Variable key pos) = do
  value <- getVariable pos root key
  pure $ Argument value pos
replaceVariable _ a = pure a
