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
                                                          DataType (..))
import           Data.Morpheus.Types.Internal.DataD      (AppD (..), ConsD (..), FieldD (..), TypeD (..),
                                                          gqlToHSWrappers)
import           Data.Morpheus.Types.Internal.Validation (Validation)

renderTHTypes :: [(Text, DataFullType)] -> Validation [TypeD]
renderTHTypes x = concat <$> traverse renderTHType x

renderTHType :: (Text, DataFullType) -> Validation [TypeD]
renderTHType (_, x) = genType x
  where
    genArgumentType :: (Text, DataField [(Text, DataField ())]) -> Validation [TypeD]
    genArgumentType (_, DataField {fieldArgs = []}) = pure []
    genArgumentType (fieldName, DataField {fieldArgs}) =
      pure [TypeD {tName, tCons = [ConsD {cName = tName, cFields = map genField fieldArgs}]}]
      where
        tName = "Arg" <> capital (unpack fieldName)
        -------------------------------------------
    genRecordCon name fields = ConsD {cName = unpack name, cFields = map genField fields}
    ---------------------------------------------------------------------------------------------
    genField :: (Text, DataField a) -> FieldD
    genField (key, DataField {fieldType, fieldTypeWrappers}) = FieldD (unpack key) fType
      where
        fType = gqlToHSWrappers fieldTypeWrappers (unpack fieldType)
    --------------------------------------------
    genType (Leaf (LeafEnum DataType {typeName, typeData})) =
      pure [TypeD {tName = unpack typeName, tCons = map enumOption typeData}]
      where
        enumOption name = ConsD {cName = unpack name, cFields = []}
    genType (Leaf _) = internalError "Scalar Types should defined By Native Haskell Types"
    genType (InputUnion _) = internalError "Input Unions not Supported"
    genType (InputObject DataType {typeName, typeData}) =
      pure [TypeD {tName = unpack typeName, tCons = [genRecordCon typeName typeData]}]
    genType (OutputObject DataType {typeName, typeData}) = do
      subTypes <- concat <$> traverse genArgumentType typeData
      pure $ TypeD {tName = unpack typeName, tCons = [genRecordCon typeName typeData]} : subTypes
    genType (Union DataType {typeName, typeData}) = do
      let tCons = map unionCon typeData
      pure [TypeD {tName = unpack typeName, tCons}]
      where
        unionCon DataField {fieldType} =
          ConsD {cName, cFields = [FieldD {fieldNameD = "un" <> cName, fieldTypeD = BaseD utName}]}
          where
            cName = unpack typeName <> utName
            utName = unpack fieldType
    ------------------------------------------------------------------------------------------------------------
