{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.Morpheus.PreProcess
    ( preProcessQuery
    )
where

import           Data.List                      ( find )
import qualified Data.Map                      as M
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.Morpheus.Types.Types      ( Validation(..)
                                                , (::->)(..)
                                                , QuerySelection(..)
                                                , SelectionSet
                                                , Arguments(..)
                                                , FragmentLib
                                                , Fragment(..)
                                                , Argument(..)
                                                , GQLQueryRoot(..)
                                                , EnumOf(..)
                                                )
import           Data.Morpheus.Types.JSType     (JSType(..))
import           Data.Morpheus.Types.MetaInfo   (MetaInfo(..))
import           Data.Morpheus.ErrorMessage     ( semanticError
                                                , handleError
                                                , cannotQueryField
                                                , requiredArgument
                                                ,unknownFragment
                                                , variableIsNotDefined
                                                , unsupportedArgumentType
                                                , invalidEnumOption
                                                )
import           Data.Morpheus.Schema.GQL__TypeKind (GQL__TypeKind(..))
import           Data.Morpheus.Schema.GQL__EnumValue (isEnumOf)
import           Data.Proxy
import           Data.Morpheus.Types.Introspection
                                                ( GQL__Type(..)
                                                , GQL__Field
                                                , emptyLib
                                                , GQLTypeLib
                                                , GQL__InputValue
                                                )
import           Data.Morpheus.Schema.SchemaField
                                                ( getFieldTypeByKey
                                                , fieldArgsByKey
                                                )
import qualified Data.Morpheus.Schema.GQL__Type as T
import qualified Data.Morpheus.Schema.InputValue as I (name,inputValueMeta,isRequired, typeName )

existsType :: Text -> GQLTypeLib -> Validation GQL__Type
existsType typeName typeLib = case M.lookup typeName typeLib of
    Nothing -> handleError $ pack $ "type does not exist" ++ unpack typeName
    Just x  -> pure x

checkQueryVariables :: GQLTypeLib  -> GQLQueryRoot -> [(Text,Argument)] -> Validation [(Text,Argument)]
checkQueryVariables typeLib root = mapM (checkVariableType  typeLib)

checkVariableType :: GQLTypeLib -> (Text,Argument) -> Validation (Text,Argument)
checkVariableType typeLib ( key, Variable typeName)  = existsType typeName typeLib >>= checkType
    where
       checkType _type = case T.kind _type of
            EnumOf SCALAR -> pure (key, Variable typeName)
            EnumOf INPUT_OBJECT -> pure (key, Variable typeName)
            _ -> Left  $ unsupportedArgumentType MetaInfo {
                           className= typeName,
                           cons = "",
                           key = key
            }


-- TODO: replace all var types with Variable values
replaceVariable :: GQLQueryRoot -> Argument -> Validation Argument
replaceVariable root (Variable key) =
    case M.lookup key (inputVariables root) of
        Nothing    -> Left  $ variableIsNotDefined  $ MetaInfo {
          className="TODO: Name",
          cons = "",
          key = key
        }
        Just value -> pure $ Argument value
replaceVariable _ x = pure x

validateEnum :: GQL__Type -> Argument -> Validation Argument
validateEnum _type (Argument (JSEnum argument)) = if isEnumOf argument (unwrapField $ T.enumValues _type)  then  pure (Argument (JSEnum argument)) else error
  where   unwrapField (Some x) = x
          error = Left $  invalidEnumOption $ MetaInfo (T.name _type) "" argument

-- TODO: Validate other Types , INPUT_OBJECT
checkArgumentType :: GQLTypeLib -> Text -> Argument -> Validation Argument
checkArgumentType typeLib typeName argument  = existsType typeName typeLib >>= checkType
    where
      checkType _type = case T.kind _type of
        EnumOf ENUM -> validateEnum _type argument
        _ -> pure argument

validateArgument
    :: GQLTypeLib -> GQLQueryRoot -> Arguments -> GQL__InputValue -> Validation (Text, Argument)
validateArgument types root requestArgs inpValue =
    case lookup (I.name inpValue) requestArgs of
        Nothing -> if  I.isRequired inpValue
            then Left $ requiredArgument $ I.inputValueMeta inpValue
            else pure (key, Argument JSNull)
        Just x -> replaceVariable root x >>= checkArgumentType types  (I.typeName inpValue) >>= validated
    where
       key = I.name inpValue
       validated x = pure (key, x)

-- TODO: throw Error when gql request has more arguments al then inputType
validateArguments
    :: GQLTypeLib -> GQLQueryRoot -> [GQL__InputValue] -> Arguments -> Validation Arguments
validateArguments typeLib root inputs args = mapM (validateArgument typeLib root args) inputs

fieldOf :: GQL__Type -> Text -> Validation GQL__Type
fieldOf _type fieldName = case getFieldTypeByKey fieldName _type of
    Nothing    -> Left $ cannotQueryField $ MetaInfo {
      key = fieldName
      , cons = ""
      , className = T.name _type
    }
    Just fieldType -> pure fieldType

validateSpread :: FragmentLib -> Text -> Validation [(Text, QuerySelection)]
validateSpread frags key = case M.lookup key frags of
    Nothing -> Left $ unknownFragment $ MetaInfo {
                className = ""
                , cons      = ""
                , key       = key
             }
    Just (Fragment _ _ (SelectionSet _ gqlObj)) -> pure gqlObj

propagateSpread
    :: GQLQueryRoot -> (Text, QuerySelection) -> Validation [(Text, QuerySelection)]
propagateSpread root (key , Spread _) = validateSpread (fragments root) key
propagateSpread root (text, value     ) = pure [(text, value)]

typeBy typeLib _parentType _name = fieldOf _parentType _name >>= fieldType
    where fieldType field = existsType (T.name field) typeLib

argsType :: GQL__Type -> Text -> Validation [GQL__InputValue]
argsType currentType key = case fieldArgsByKey key currentType of
    Nothing   -> handleError $ pack $ "header not found: " ++ show key
    Just args -> pure args

mapSelectors
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> SelectionSet
    -> Validation SelectionSet
mapSelectors typeLib root _type selectors = do
  selectors' <- concat <$> mapM (propagateSpread root) selectors

  mapM (validateBySchema typeLib root _type) selectors'

validateBySchema
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> (Text, QuerySelection)
    -> Validation (Text, QuerySelection)
validateBySchema typeLib root _parentType (_name, SelectionSet head selectors)
    = do
        _type      <- typeBy typeLib _parentType _name
        _argsType  <- argsType _parentType _name
        head'      <- validateArguments typeLib root _argsType head
        selectors' <- mapSelectors typeLib root _type selectors
        pure (_name, SelectionSet head' selectors')

validateBySchema typeLib root _parentType (_name, Field head field) = do
    _checksIfHasType  <- typeBy typeLib _parentType _name
    _argsType <- argsType _parentType _name
    head'     <- validateArguments typeLib root _argsType head
    pure (_name, Field head' field)

validateBySchema _ _ _ x = pure x

preProcessQuery :: GQLTypeLib -> GQLQueryRoot -> Validation QuerySelection
preProcessQuery lib root = do
    _type <- existsType "Query" lib
    let (SelectionSet args body) = queryBody root
    variable <- checkQueryVariables lib root args
    selectors <- mapSelectors lib root _type body
    pure $ SelectionSet [] selectors
