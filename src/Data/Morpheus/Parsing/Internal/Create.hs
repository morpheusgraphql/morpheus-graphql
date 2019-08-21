{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Internal.Create where

import           Data.Morpheus.Parsing.Internal.Terms (parseAssignment, parseMaybeTuple, parseNonNull, parseWrappedType,
                                                       pipeLiteral, qualifier, setOf, spaceAndComments, token)
import           Data.Morpheus.Types.Internal.Data    (DataArgument, DataField (..), DataFingerprint (..),
                                                       DataFullType (..), DataLeaf (..), DataOutputField, DataType (..),
                                                       DataTypeLib (..), DataTypeWrapper, DataValidator (..), Key,
                                                       defineType, initTypeLib)
import           Data.Text                            (Text)

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

createArgument fieldName x = (fieldName, createField () fieldName x)

createDataTypeLib :: Monad m => [(Text, DataFullType)] -> m DataTypeLib
createDataTypeLib types =
  case takeByKey "Query" types of
    (Just query, lib1) ->
      case takeByKey "Mutation" lib1 of
        (mutation, lib2) ->
          case takeByKey "Subscription" lib2 of
            (subscription, lib3) -> pure ((foldr defineType (initTypeLib query) lib3) {mutation, subscription})
    _ -> fail "Query Not Defined"
  ----------------------------------------------------------------------------
  where
    takeByKey key lib =
      case lookup key lib of
        Just (OutputObject value) -> (Just (key, value), filter ((/= key) . fst) lib)
        _                         -> (Nothing, lib)
