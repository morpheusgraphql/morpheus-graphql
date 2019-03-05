{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Variable
    ( checkQueryVariables
    , replaceVariable
    )
where


import           Data.Text                      ( Text )
import qualified Data.Map                      as M
import           Data.Morpheus.Types.Introspection
                                                ( GQLTypeLib )
import           Data.Morpheus.Types.Types      ( Validation(..)
                                                , Arguments(..)
                                                , Argument(..)
                                                , GQLQueryRoot(..)
                                                , EnumOf(..)
                                                )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T
import qualified Data.Morpheus.Schema.InputValue
                                               as I
import           Data.Morpheus.Schema.GQL__TypeKind
                                                ( GQL__TypeKind(..) )
import           Data.Morpheus.PreProcess.Utils ( existsType )
import           Data.Morpheus.ErrorMessage     ( unsupportedArgumentType
                                                , variableIsNotDefined
                                                )
import           Data.Morpheus.PreProcess.InputObject
                                                ( validateInputObject )


getVariable :: GQLQueryRoot -> Text -> Validation JSType
getVariable root key = case M.lookup key (inputVariables root) of
    Nothing -> Left $ variableIsNotDefined $ MetaInfo
        { className = "TODO: Name"
        , cons      = ""
        , key       = key
        }
    Just value -> pure value

checkVariableType
    :: GQLTypeLib
    -> GQLQueryRoot
    -> (Text, Argument)
    -> Validation (Text, Argument)
checkVariableType typeLib root (key, Variable typeName) =
    existsType typeName typeLib >>= checkType
  where
    checkType _type = case T.kind _type of
        EnumOf SCALAR       -> pure (key, Variable typeName)
        EnumOf INPUT_OBJECT -> pure (key, Variable typeName)
        _                   -> Left $ unsupportedArgumentType MetaInfo
            { className = typeName
            , cons      = ""
            , key       = key
            }

checkQueryVariables
    :: GQLTypeLib
    -> GQLQueryRoot
    -> [(Text, Argument)]
    -> Validation [(Text, Argument)]
checkQueryVariables typeLib root = mapM (checkVariableType typeLib root)


replaceVariable :: GQLQueryRoot -> Argument -> Validation Argument
replaceVariable root (Variable key) = Argument <$> getVariable root key
