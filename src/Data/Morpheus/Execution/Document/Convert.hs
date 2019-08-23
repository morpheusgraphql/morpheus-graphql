{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Execution.Document.Convert
  ( renderTHTypes
  ) where

import           Data.Semigroup                          ((<>))
import           Data.Text                               (Text, unpack)

--
-- MORPHEUS
import           Data.Morpheus.Error.Internal            (internalError)
import           Data.Morpheus.Execution.Internal.Utils  (capital)
import           Data.Morpheus.Types.Internal.Data       (DataField (..), DataFullType (..), DataLeaf (..),
                                                          DataOutputField, DataType (..), DataTypeKind (..))
import           Data.Morpheus.Types.Internal.DataD      (AppD (..), ConsD (..), FieldD (..), GQLTypeD, TypeD (..),
                                                          gqlToHSWrappers)
import           Data.Morpheus.Types.Internal.Validation (Validation)

renderTHTypes :: [(Text, DataFullType)] -> Validation [GQLTypeD]
renderTHTypes = traverse renderTHType

renderTHType :: (Text, DataFullType) -> Validation GQLTypeD
renderTHType (_, x) = genType x
  where
    genArgumentType :: (Text, DataField [(Text, DataField ())]) -> Validation [TypeD]
    genArgumentType (_, DataField {fieldArgs = []}) = pure []
    genArgumentType (fieldName, DataField {fieldArgs}) =
      pure [TypeD {tName, tCons = [ConsD {cName = tName, cFields = map genField fieldArgs}]}]
      where
        tName = "Arg" <> capital (unpack fieldName)
    ---------------------------------------------------------------------------------------------
    genField :: (Text, DataField a) -> FieldD
    genField (key, DataField {fieldType, fieldTypeWrappers}) = FieldD (unpack key) fType
      where
        fType = gqlToHSWrappers fieldTypeWrappers (unpack fieldType)
    ---------------------------------------------------------------------------------------------
    genResField :: (Text, DataOutputField) -> FieldD
    genResField (key, DataField {fieldType, fieldTypeWrappers}) = FieldD (unpack key) fType
      where
        fType = FuncD "()" $ gqlToHSWrappers fieldTypeWrappers (unpack fieldType)
    --------------------------------------------
    genType (Leaf (LeafEnum DataType {typeName, typeData})) =
      pure (TypeD {tName = unpack typeName, tCons = map enumOption typeData}, KindEnum, [])
      where
        enumOption name = ConsD {cName = unpack name, cFields = []}
    genType (Leaf _) = internalError "Scalar Types should defined By Native Haskell Types"
    genType (InputUnion _) = internalError "Input Unions not Supported"
    genType (InputObject DataType {typeName, typeData}) =
      pure
        ( TypeD {tName = unpack typeName, tCons = [ConsD {cName = unpack typeName, cFields = map genField typeData}]}
        , KindInputObject
        , [])
    genType (OutputObject DataType {typeName, typeData}) = do
      subTypes <- concat <$> traverse genArgumentType typeData
      pure
        ( TypeD {tName = unpack typeName, tCons = [ConsD {cName = unpack typeName, cFields = map genResField typeData}]}
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
    ------------------------------------------------------------------------------------------------------------
