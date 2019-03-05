{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Arguments
    ( validateArguments
    , checkQueryVariables
    )
where


import           Data.Text                      ( Text )
import qualified Data.Map                      as M
import           Data.Morpheus.Types.Introspection
                                                ( GQL__Type(..)
                                                , GQLTypeLib
                                                , GQL__InputValue
                                                )
import           Data.Morpheus.Types.Types      ( Validation(..)
                                                , (::->)(Some)
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
import           Data.Morpheus.ErrorMessage     ( invalidEnumOption
                                                , unsupportedArgumentType
                                                , variableIsNotDefined
                                                , requiredArgument
                                                )
import           Data.Morpheus.Schema.GQL__EnumValue
                                                ( isEnumOf )

validateEnum :: GQL__Type -> Argument -> Validation Argument
validateEnum _type (Argument (JSEnum argument)) =
    if isEnumOf argument (unwrapField $ T.enumValues _type)
        then pure (Argument (JSEnum argument))
        else error
  where
    unwrapField (Some x) = x
    error = Left $ invalidEnumOption $ MetaInfo (T.name _type) "" argument



checkVariableType
    :: GQLTypeLib -> (Text, Argument) -> Validation (Text, Argument)
checkVariableType typeLib (key, Variable typeName) =
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
checkQueryVariables typeLib root = mapM (checkVariableType typeLib)

-- TODO: replace all var types with Variable values
replaceVariable :: GQLQueryRoot -> Argument -> Validation Argument
replaceVariable root (Variable key) =
    case M.lookup key (inputVariables root) of
        Nothing -> Left $ variableIsNotDefined $ MetaInfo
            { className = "TODO: Name"
            , cons      = ""
            , key       = key
            }
        Just value -> pure $ Argument value
replaceVariable _ x = pure x


-- TODO: Validate other Types , INPUT_OBJECT
checkArgumentType :: GQLTypeLib -> Text -> Argument -> Validation Argument
checkArgumentType typeLib typeName argument =
    existsType typeName typeLib >>= checkType
  where
    checkType _type = case T.kind _type of
        EnumOf ENUM -> validateEnum _type argument
        _           -> pure argument

validateArgument
    :: GQLTypeLib
    -> GQLQueryRoot
    -> Arguments
    -> GQL__InputValue
    -> Validation (Text, Argument)
validateArgument types root requestArgs inpValue =
    case lookup (I.name inpValue) requestArgs of
        Nothing -> if I.isRequired inpValue
            then Left $ requiredArgument $ I.inputValueMeta inpValue
            else pure (key, Argument JSNull)
        Just x ->
            replaceVariable root x
                >>= checkArgumentType types (I.typeName inpValue)
                >>= validated
  where
    key = I.name inpValue
    validated x = pure (key, x)


-- TODO: throw Error when gql request has more arguments al then inputType
validateArguments
    :: GQLTypeLib
    -> GQLQueryRoot
    -> [GQL__InputValue]
    -> Arguments
    -> Validation Arguments
validateArguments typeLib root inputs args =
    mapM (validateArgument typeLib root args) inputs
