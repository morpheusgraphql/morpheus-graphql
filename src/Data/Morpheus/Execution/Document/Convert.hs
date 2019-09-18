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
import           Data.Morpheus.Types.Internal.Data       (DataField (..), DataFullType (..), DataLeaf (..),
                                                          DataOutputField, DataType (..), DataTypeKind (..))
import           Data.Morpheus.Types.Internal.DataD      (AppD (..), ConsD (..), FieldD (..), GQLTypeD (..), KindD (..),
                                                          ResolverKind (..), TypeD (..), gqlToHSWrappers)
import           Data.Morpheus.Types.Internal.Validation (Validation)

renderTHTypes :: [(Text, DataFullType)] -> Validation [GQLTypeD]
renderTHTypes lib = traverse renderTHType lib
  where
    getFieldType key =
      case lookup key lib of
        Nothing              -> ExternalResolver
        Just OutputObject {} -> TypeVarResolver
        Just Union {}        -> TypeVarResolver
        Just _               -> PlainResolver
    renderTHType :: (Text, DataFullType) -> Validation GQLTypeD
    renderTHType (_, x) = genType x
      where
        argsTypeName fieldName = capital (unpack fieldName) <> "Args"
        genArgumentType :: (Text, DataField) -> Validation [TypeD]
        genArgumentType (_, DataField {fieldArgs = []}) = pure []
        genArgumentType (fieldName, DataField {fieldArgs}) =
          pure [TypeD {tName, tCons = [ConsD {cName = tName, cFields = map genField fieldArgs}]}]
          where
            tName = argsTypeName fieldName
        -------------------------------------------
        genFieldTypeName "String" = "Text"
        genFieldTypeName name     = unpack name
        ---------------------------------------------------------------------------------------------
        genField :: (Text, DataField) -> FieldD
        genField (key, DataField {fieldType, fieldTypeWrappers}) =
          FieldD {fieldNameD = unpack key, fieldTypeD, fieldArgsD = Nothing}
          where
            fieldTypeD = gqlToHSWrappers fieldTypeWrappers (genFieldTypeName fieldType)
        ---------------------------------------------------------------------------------------------
        genResField :: (Text, DataOutputField) -> FieldD
        genResField (key, DataField {fieldName, fieldArgs, fieldType, fieldTypeWrappers}) =
          FieldD {fieldNameD = unpack key, fieldTypeD, fieldArgsD}
          where
            fieldArgsD = Just (argsTName fieldArgs, getFieldType $ pack $ genFieldTypeName fieldType)
            fieldTypeD = gqlToHSWrappers fieldTypeWrappers (genFieldTypeName fieldType)
            argsTName [] = "()"
            argsTName _  = argsTypeName fieldName
        --------------------------------------------
        genType (Leaf (LeafEnum DataType {typeName, typeData})) =
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
        genType (InputObject DataType {typeName, typeData}) =
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
        genType (OutputObject DataType {typeName, typeData}) = do
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
        genType (Union DataType {typeName, typeData}) = do
          let tCons = map unionCon typeData
          pure
            GQLTypeD {typeD = TypeD {tName = unpack typeName, tCons}, typeKindD = RegularKindD KindUnion, typeArgD = []}
          where
            unionCon DataField {fieldType} =
              ConsD
                { cName
                , cFields = [FieldD {fieldNameD = "un" <> cName, fieldArgsD = Nothing, fieldTypeD = BaseD utName}]
                }
              where
                cName = unpack typeName <> utName
                utName = unpack fieldType
