{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Internal.Create where

import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (parseAssignment, parseMaybeTuple, parseNonNull,
                                                          parseWrappedType, pipeLiteral, qualifier, setOf,
                                                          spaceAndComments, token)
import           Data.Morpheus.Types.Internal.Data       (DataArgument, DataField (..), DataFingerprint (..),
                                                          DataFullType (..), DataLeaf (..), DataOutputField,
                                                          DataType (..), DataTypeWrapper, DataValidator (..), Key)
import           Data.Text                               (Text)
import           Text.Megaparsec                         (label, sepBy1, (<|>))
import           Text.Megaparsec.Char                    (char, space1, string)

createField :: a -> Text -> ([DataTypeWrapper], Text) -> DataField a
createField fieldArgs fieldName (fieldTypeWrappers, fieldType) =
  DataField {fieldArgs, fieldName, fieldType, fieldTypeWrappers, fieldHidden = False}

createType :: Text -> a -> DataType a
createType typeName typeData =
  DataType {typeName, typeDescription = "", typeFingerprint = SystemFingerprint "", typeVisibility = True, typeData}

createScalarType typeName = (typeName, Leaf $ CustomScalar $ createType typeName (DataValidator pure))

createEnumType typeName typeData = (typeName, Leaf $ LeafEnum $ createType typeName typeData)

createUnionType typeName typeData = (typeName, Union $ createType typeName $ map unionField typeData)
  where
    unionField fieldType = createField () "" ([], fieldType)

createArgument fieldName wrappers fieldType = (fieldName, createField () fieldName (wrappers, fieldType))