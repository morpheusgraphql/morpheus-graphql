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
import           Data.Morpheus.Types.Internal.DataD      (AppD (..), ConsD (..), FieldD (..), GQLTypeD,
                                                          ResolverKind (..), TypeD (..), gqlToHSWrappers)
import           Data.Morpheus.Types.Internal.Validation (Validation)

renderTHTypes :: [(Text, DataFullType)] -> Validation [GQLTypeD]
renderTHTypes lib = traverse renderTHType lib
  where
    getFieldType key =
      case lookup key lib of
        Nothing              -> ExternalResolver
        Just OutputObject {} -> TypeVarResolver
        Just InputUnion {}   -> TypeVarResolver
        Just _               -> PlainResolver
    renderTHType :: (Text, DataFullType) -> Validation GQLTypeD
    renderTHType (_, x) = genType x
      where
        argsTypeName fieldName = capital (unpack fieldName) <> "Args"
        genArgumentType :: (Text, DataField [(Text, DataField ())]) -> Validation [TypeD]
        genArgumentType (_, DataField {fieldArgs = []}) = pure []
        genArgumentType (fieldName, DataField {fieldArgs}) =
          pure [TypeD {tName, tCons = [ConsD {cName = tName, cFields = map genField fieldArgs}]}]
          where
            tName = argsTypeName fieldName
        -------------------------------------------
        genFieldTypeName "String" = "Text"
        genFieldTypeName name     = unpack name
        ---------------------------------------------------------------------------------------------
        genField :: (Text, DataField a) -> FieldD
        genField (key, DataField {fieldType, fieldTypeWrappers}) = FieldD (unpack key) fType
          where
            fType = gqlToHSWrappers fieldTypeWrappers (genFieldTypeName fieldType)
        ---------------------------------------------------------------------------------------------
        genResField :: (Text, DataOutputField) -> FieldD
        genResField (key, DataField {fieldName, fieldArgs, fieldType, fieldTypeWrappers}) = FieldD (unpack key) fType
          where
            fType =
              ResD (argsTName fieldArgs) (getFieldType $ pack $ genFieldTypeName fieldType) $
              gqlToHSWrappers fieldTypeWrappers (genFieldTypeName fieldType)
            argsTName [] = "()"
            argsTName _  = argsTypeName fieldName
        --------------------------------------------
        genType (Leaf (LeafEnum DataType {typeName, typeData})) =
          pure (TypeD {tName = unpack typeName, tCons = map enumOption typeData}, KindEnum, [])
          where
            enumOption name = ConsD {cName = unpack name, cFields = []}
        genType (Leaf _) = internalError "Scalar Types should defined By Native Haskell Types"
        genType (InputUnion _) = internalError "Input Unions not Supported"
        genType (InputObject DataType {typeName, typeData}) =
          pure
            ( TypeD
                {tName = unpack typeName, tCons = [ConsD {cName = unpack typeName, cFields = map genField typeData}]}
            , KindInputObject
            , [])
        genType (OutputObject DataType {typeName, typeData}) = do
          subTypes <- concat <$> traverse genArgumentType typeData
          pure
            ( TypeD
                {tName = unpack typeName, tCons = [ConsD {cName = unpack typeName, cFields = map genResField typeData}]}
            , KindObject
            , subTypes)
        genType (Union DataType {typeName, typeData}) = do
          let tCons = map unionCon typeData
          pure (TypeD {tName = unpack typeName, tCons}, KindUnion, [])
          where
            unionCon DataField {fieldType} =
              ConsD {cName, cFields = [FieldD {fieldNameD = "un" <> cName, fieldTypeD = BaseD utName}]}
              where
                cName = unpack typeName <> utName
                utName = unpack fieldType
