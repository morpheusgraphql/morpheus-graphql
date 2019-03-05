{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Arguments
    ( validateArguments
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
import           Data.Morpheus.PreProcess.Enum  ( validateEnum )
import           Data.Morpheus.PreProcess.Variable
                                                ( replaceVariable )

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


checkForUnknownArguments
    :: [GQL__InputValue] -> Arguments -> Validation [GQL__InputValue]
checkForUnknownArguments inputs args = pure inputs


-- TODO: throw Error when gql request has more arguments al then inputType
validateArguments
    :: GQLTypeLib
    -> GQLQueryRoot
    -> [GQL__InputValue]
    -> Arguments
    -> Validation Arguments
validateArguments typeLib root inputs args =
    checkForUnknownArguments inputs args >>= mapM (validateArgument typeLib root args) 
