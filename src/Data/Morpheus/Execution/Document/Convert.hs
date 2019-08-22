{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Execution.Document.Convert
  ( renderTHTypes
  ) where

import           Data.Semigroup                             ((<>))
import           Data.Text                                  (Text, unpack)

--
-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataFullType (..), DataLeaf (..),
                                                             DataType (..), DataTypeLib (..), DataTypeWrapper,
                                                             allDataTypes)
import           Data.Morpheus.Types.Internal.DataD         (ConsD (..), FieldD (..), TypeD (..), gqlToHSWrappers)
import           Data.Morpheus.Types.Internal.Validation    (Validation)

renderTHTypes :: [(Text, DataFullType)] -> Validation [TypeD]
renderTHTypes x = concat <$> traverse renderTHType x

renderTHType :: (Text, DataFullType) -> Validation [TypeD]
renderTHType (_, x) = genType x
  --  genInputType :: Text -> Validation [TypeD]
  --  genInputType name = name >>= subTypes
  --    where
  --      subTypes (InputObject DataType {typeName, typeData}) = do
  --      types <- concat <$> mapM toInputTypeD typeData
  --        fields <- traverse toFieldD typeData
  --        pure $ typeD fields : types
  --        where
  --          typeD fields = TypeD {tName = unpack typeName, tCons = [ConsD {cName = unpack typeName, cFields = fields}]}
  --          ---------------------------------------------------------------
  --          toInputTypeD :: (Text, DataField a) -> Validation [TypeD]
  --          toInputTypeD (_, DataField {fieldType}) = genInputType fieldType
  --          ----------------------------------------------------------------
  --          toFieldD :: (Text, DataField a) -> Validation FieldD
  --          toFieldD (key, DataField {fieldType, fieldTypeWrappers}) = do
  --            fType <- typeFrom <$> getType lib fieldType
  --            pure $ FieldD (unpack key) (wrType fType)
  --            where
  --              wrType fieldT = gqlToHSWrappers fieldTypeWrappers (unpack fieldT)
  --      subTypes (Leaf x) = buildLeaf x
  --      subTypes _ = pure []
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
    genType (Leaf x) = buildLeaf x
    -- genType (InputObject x) = typeName x
    genType (OutputObject DataType {typeName, typeData}) = do
      (con, subTypes) <- genRecordCon typeName typeData
      pure $ TypeD {tName = unpack typeName, tCons = [con]} : subTypes
    --genType (Union x) = typeName x
    ------------------------------------------------------------------------------------------------------------

buildLeaf :: DataLeaf -> Validation [TypeD]
buildLeaf (LeafEnum DataType {typeName, typeData}) =
  pure [TypeD {tName = unpack typeName, tCons = map enumOption typeData}]
  where
    enumOption name = ConsD {cName = unpack name, cFields = []}
buildLeaf _ = pure []
