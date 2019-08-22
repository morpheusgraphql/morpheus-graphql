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
import           Data.Morpheus.Types.Internal.Data       (DataField (..), DataFullType (..), DataLeaf (..),
                                                          DataType (..), DataTypeWrapper, allDataTypes)
import           Data.Morpheus.Types.Internal.DataD      (AppD (..), ConsD (..), FieldD (..), TypeD (..),
                                                          gqlToHSWrappers)
import           Data.Morpheus.Types.Internal.Validation (Validation)

renderTHTypes :: [(Text, DataFullType)] -> Validation [TypeD]
renderTHTypes x = concat <$> traverse renderTHType x

renderTHType :: (Text, DataFullType) -> Validation [TypeD]
renderTHType (_, x) = genType x
    -------------------------------------------
  {- rootArguments :: Text -> Validation [TypeD]
    rootArguments name = do
      types <- concat <$> mapM (genInputType . variableType . snd) variables
      pure $ typeD : types
      where
        typeD :: TypeD
        typeD = TypeD {tName = unpack name, tCons = [ConsD {cName = unpack name, cFields = map fieldD variables}]}
        ---------------------------------------
        fieldD :: (Text, Variable ()) -> FieldD
        fieldD (key, Variable {variableType, variableTypeWrappers}) = FieldD (unpack key) wrType
          where
            wrType = gqlToHSWrappers variableTypeWrappers (unpack variableType)
    ------------------------------------------- -}
  where
    genRecordCon name fields = do
      let cFields = map genField fields
     -- subTypes <- newFieldTypes dataType
      pure (ConsD {cName = unpack name, cFields}, []) {-subTypes-}
      ---------------------------------------------------------------------------------------------
      where
        genField :: (Text, DataField a) -> FieldD
        genField (fName, DataField {fieldType, fieldTypeWrappers}) = FieldD (unpack fName) fType
          where
            fType = gqlToHSWrappers fieldTypeWrappers (unpack fieldType)
    --------------------------------------------
    genType (Leaf (LeafEnum DataType {typeName, typeData})) =
      pure [TypeD {tName = unpack typeName, tCons = map enumOption typeData}]
      where
        enumOption name = ConsD {cName = unpack name, cFields = []}
    genType (Leaf _) = internalError "Scalar Types should defined By Native Haskell Types"
    genType (InputUnion _) = internalError "Input Unions not Supported"
    genType (InputObject DataType {typeName, typeData}) = do
      (con, _) <- genRecordCon typeName typeData
      pure [TypeD {tName = unpack typeName, tCons = [con]}]
    genType (OutputObject DataType {typeName, typeData}) = do
      (con, subTypes) <- genRecordCon typeName typeData
      pure $ TypeD {tName = unpack typeName, tCons = [con]} : subTypes
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
