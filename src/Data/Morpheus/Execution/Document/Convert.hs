{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Execution.Document.Convert
  ( renderTHTypes
  ) where

import           Data.Semigroup                          ((<>))
import           Data.Text                               (Text, pack, unpack)

--
-- MORPHEUS
import           Data.Morpheus.Error.Internal            (internalError)
import           Data.Morpheus.Execution.Internal.Utils  (capital)
import           Data.Morpheus.Types.Internal.Data       (ArgsType (..), DataField (..), DataField, DataFullType (..),
                                                          DataLeaf (..), DataTyCon (..), DataTypeKind (..), KindD (..),
                                                          ResolverKind (..), TypeAlias (..))
import           Data.Morpheus.Types.Internal.DataD      (ConsD (..), GQLTypeD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)

renderTHTypes :: Bool -> [(Text, DataFullType)] -> Validation [GQLTypeD]
renderTHTypes namespace lib = traverse renderTHType lib
  where
    renderTHType :: (Text, DataFullType) -> Validation GQLTypeD
    renderTHType (tyConName, x) = genType x
      where
        genArgsTypeName fieldName
          | namespace = unpack tyConName <> argTName
          | otherwise = argTName
          where
            argTName = capital fieldName <> "Args"
        genArgumentType :: (Text, DataField) -> Validation [TypeD]
        genArgumentType (_, DataField {fieldArgs = []}) = pure []
        genArgumentType (fieldName, DataField {fieldArgs}) =
          pure [TypeD {tName, tCons = [ConsD {cName = tName, cFields = map genField fieldArgs}]}]
          where
            tName = genArgsTypeName $ unpack fieldName
        -------------------------------------------
        genFieldTypeName "String" = "Text"
        genFieldTypeName name     = name
        ---------------------------------------------------------------------------------------------
        genField :: (Text, DataField) -> DataField
        genField (_, field@DataField {fieldType = alias@TypeAlias {aliasTyCon}}) =
          field {fieldType = alias {aliasTyCon = genFieldTypeName aliasTyCon}}
        ---------------------------------------------------------------------------------------------
        genResField :: (Text, DataField) -> DataField
        genResField (_, field@DataField {fieldName, fieldArgs, fieldType = alias@TypeAlias {aliasTyCon}}) =
          field {fieldType = alias {aliasTyCon = ftName, aliasArgs}, fieldArgsType}
          where
            ftName = genFieldTypeName aliasTyCon
            ---------------------------------------
            aliasArgs =
              case lookup aliasTyCon lib of
                Just OutputObject {} -> Just "m"
                Just Union {}        -> Just "m"
                _                    -> Nothing
            -----------------------------------
            fieldArgsType = Just $ ArgsType {argsTypeName, resKind = getFieldType ftName}
              where
                argsTypeName
                  | null fieldArgs = "()"
                  | otherwise = pack $ genArgsTypeName $ unpack fieldName
                --------------------------------------
                getFieldType key =
                  case lookup key lib of
                    Nothing              -> ExternalResolver
                    Just OutputObject {} -> TypeVarResolver
                    Just Union {}        -> TypeVarResolver
                    Just _               -> PlainResolver
        --------------------------------------------
        genType (Leaf (LeafEnum DataTyCon {typeName, typeData})) =
          pure
            GQLTypeD
              { typeD = TypeD {tName = unpack typeName, tCons = map enumOption typeData}
              , typeKindD = RegularKindD KindEnum
              , typeArgD = []
              }
          where
            enumOption name = ConsD {cName = unpack name, cFields = []}
        genType (Leaf _) = internalError "Scalar Types should defined By Native Haskell Types"
        genType (InputUnion _) = internalError "Input Unions not Supported"
        genType (InputObject DataTyCon {typeName, typeData}) =
          pure
            GQLTypeD
              { typeD =
                  TypeD
                    { tName = unpack typeName
                    , tCons = [ConsD {cName = unpack typeName, cFields = map genField typeData}]
                    }
              , typeKindD = RegularKindD KindInputObject
              , typeArgD = []
              }
        genType (OutputObject DataTyCon {typeName, typeData}) = do
          typeArgD <- concat <$> traverse genArgumentType typeData
          pure
            GQLTypeD
              { typeD =
                  TypeD
                    { tName = unpack typeName
                    , tCons = [ConsD {cName = unpack typeName, cFields = map genResField typeData}]
                    }
              , typeKindD =
                  if typeName == "Subscription"
                    then SubscriptionD
                    else RegularKindD KindObject
              , typeArgD
              }
        genType (Union DataTyCon {typeName, typeData}) = do
          let tCons = map unionCon typeData
          pure
            GQLTypeD {typeD = TypeD {tName = unpack typeName, tCons}, typeKindD = RegularKindD KindUnion, typeArgD = []}
          where
            unionCon field@DataField {fieldType} =
              ConsD
                { cName
                , cFields =
                    [ field
                        { fieldName = pack $ "un" <> cName
                        , fieldType = TypeAlias {aliasTyCon = pack utName, aliasArgs = Just "m", aliasWrappers = []}
                        }
                    ]
                }
              where
                cName = unpack typeName <> utName
                utName = unpack $ aliasTyCon fieldType
