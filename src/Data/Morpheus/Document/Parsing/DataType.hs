{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.Parsing.DataType
  ( parseDataType
  ) where

import           Data.Morpheus.Parsing.Internal.Terms2 (Parser, nonNull, parseAssignment, parseMaybeTuple, pipe,
                                                        qualifier, setOf, spaceAndComments, token, wrappedType)
import           Data.Morpheus.Types.Internal.Data     (DataArgument, DataField (..), DataFingerprint (..),
                                                        DataFullType (..), DataLeaf (..), DataOutputField,
                                                        DataType (..), DataTypeWrapper, DataValidator (..), Key)
import           Data.Text                             (Text)
import           Text.Megaparsec                       (label, sepBy1, (<|>))
import           Text.Megaparsec.Char                  (char, space1, string)

createType :: Text -> a -> DataType a
createType typeName typeData =
  DataType {typeName, typeDescription = "", typeFingerprint = SystemFingerprint "", typeVisibility = True, typeData}

createField :: a -> Text -> ([DataTypeWrapper], Text) -> DataField a
createField fieldArgs fieldName (fieldTypeWrappers, fieldType) =
  DataField {fieldArgs, fieldName, fieldType, fieldTypeWrappers, fieldHidden = False}

dataArgument :: Parser (Text, DataArgument)
dataArgument =
  label "Argument" $ do
    ((fieldName, _), (wrappers', fieldType)) <- parseAssignment qualifier wrappedType
    nonNull' <- nonNull
    pure (fieldName, createField () fieldName (nonNull' ++ wrappers', fieldType))

entries :: Parser [(Key, DataOutputField)]
entries = label "entries" $ setOf entry
  where
    fieldWithArgs =
      label "fieldWithArgs" $ do
        (name, _) <- qualifier
        args <- parseMaybeTuple dataArgument
        return (name, args)
    entry =
      label "entry" $ do
        ((fieldName, fieldArgs), (wrappers', fieldType)) <- parseAssignment fieldWithArgs wrappedType
        nonNull' <- nonNull
        return (fieldName, createField fieldArgs fieldName (nonNull' ++ wrappers', fieldType))

inputEntries :: Parser [(Key, DataArgument)]
inputEntries = label "inputEntries" $ setOf entry
  where
    entry =
      label "entry" $ do
        ((fieldName, _), (wrappers', fieldType)) <- parseAssignment qualifier wrappedType
        nonNull' <- nonNull
        return (fieldName, createField () fieldName (nonNull' ++ wrappers', fieldType))

typeDef :: Text -> Parser Text
typeDef kind = do
  _ <- string kind
  space1
  token

dataInputObject :: Parser (Text, DataFullType)
dataInputObject =
  label "inputObject" $ do
    typeName <- typeDef "input"
    typeData <- inputEntries
    pure (typeName, InputObject $ createType typeName typeData)

dataObject :: Parser (Text, DataFullType)
dataObject =
  label "object" $ do
    typeName <- typeDef "type"
    typeData <- entries
    pure (typeName, OutputObject $ createType typeName typeData)

dataScalar :: Parser (Text, DataFullType)
dataScalar =
  label "scalar" $ do
    typeName <- typeDef "scalar"
    pure (typeName, Leaf $ CustomScalar $ createType typeName (DataValidator pure))

dataEnum :: Parser (Text, DataFullType)
dataEnum =
  label "enum" $ do
    typeName <- typeDef "enum"
    typeData <- setOf token
    pure (typeName, Leaf $ LeafEnum $ createType typeName typeData)

dataUnion :: Parser (Text, DataFullType)
dataUnion =
  label "union" $ do
    typeName <- typeDef "union"
    _ <- char '='
    spaceAndComments
    typeData <- map unionField <$> unionsParser
    spaceAndComments
    pure (typeName, Union $ createType typeName typeData)
  where
    unionsParser = token `sepBy1` pipe
    unionField fieldType = createField () "" ([], fieldType)

parseDataType :: Parser (Text, DataFullType)
parseDataType = label "dataType" $ dataObject <|> dataInputObject <|> dataUnion <|> dataEnum <|> dataScalar
