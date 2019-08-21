{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.DataType
  ( parseDataType
  ) where

import           Data.Morpheus.Parsing.Internal.Create   (createArgument, createEnumType, createField, createScalarType,
                                                          createType, createUnionType)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (parseAssignment, parseMaybeTuple, parseNonNull,
                                                          parseWrappedType, pipeLiteral, qualifier, setOf,
                                                          spaceAndComments, token)
import           Data.Morpheus.Types.Internal.Data       (DataArgument, DataFullType (..), DataOutputField, Key)
import           Data.Text                               (Text)
import           Text.Megaparsec                         (label, sepBy1, (<|>))
import           Text.Megaparsec.Char                    (char, space1, string)

dataArgument :: Parser (Text, DataArgument)
dataArgument =
  label "Argument" $ do
    ((fieldName, _), (wrappers, fieldType)) <- parseAssignment qualifier parseWrappedType
    nonNull <- parseNonNull
    pure $ createArgument fieldName (nonNull ++ wrappers, fieldType)

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
  where
    inputEntries :: Parser [(Key, DataArgument)]
    inputEntries = label "inputEntries" $ setOf entry
      where
        entry =
          label "entry" $ do
            ((fieldName, _), (wrappers, fieldType)) <- parseAssignment qualifier parseWrappedType
            nonNull <- parseNonNull
            return (fieldName, createField () fieldName (nonNull ++ wrappers, fieldType))

dataObject :: Parser (Text, DataFullType)
dataObject =
  label "object" $ do
    typeName <- typeDef "type"
    typeData <- entries
    pure (typeName, OutputObject $ createType typeName typeData)
  where
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
            ((fieldName, fieldArgs), (wrappers, fieldType)) <- parseAssignment fieldWithArgs parseWrappedType
            nonNull <- parseNonNull
            return (fieldName, createField fieldArgs fieldName (nonNull ++ wrappers, fieldType))

dataScalar :: Parser (Text, DataFullType)
dataScalar =
  label "scalar" $ do
    typeName <- typeDef "scalar"
    pure $ createScalarType typeName

dataEnum :: Parser (Text, DataFullType)
dataEnum =
  label "enum" $ do
    typeName <- typeDef "enum"
    typeData <- setOf token
    pure $ createEnumType typeName typeData

dataUnion :: Parser (Text, DataFullType)
dataUnion =
  label "union" $ do
    typeName <- typeDef "union"
    _ <- char '='
    spaceAndComments
    typeData <- unionsParser
    spaceAndComments
    pure $ createUnionType typeName typeData
  where
    unionsParser = token `sepBy1` pipeLiteral

parseDataType :: Parser (Text, DataFullType)
parseDataType = label "dataType" $ dataObject <|> dataInputObject <|> dataUnion <|> dataEnum <|> dataScalar
