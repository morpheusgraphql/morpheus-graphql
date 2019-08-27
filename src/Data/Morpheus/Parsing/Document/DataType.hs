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
import           Data.Morpheus.Types.Internal.Data       (DataArgument, DataFullType (..), DataOutputField, Key,
                                                          RawDataType (..))
import           Data.Text                               (Text)
import           Text.Megaparsec                         (label, sepBy1, some, (<|>))
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
    typeData <- inputObjectEntries
    pure (typeName, InputObject $ createType typeName typeData)

inputObjectEntries :: Parser [(Key, DataArgument)]
inputObjectEntries = label "inputEntries" $ setOf entry
  where
    entry =
      label "entry" $ do
        ((fieldName, _), (wrappers, fieldType)) <- parseAssignment qualifier parseWrappedType
        nonNull <- parseNonNull
        return (fieldName, createField () fieldName (nonNull ++ wrappers, fieldType))

outputObjectEntries :: Parser [(Key, DataOutputField)]
outputObjectEntries = label "entries" $ setOf entry
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

dataObject :: Parser (Text, RawDataType)
dataObject =
  label "object" $ do
    typeName <- typeDef "type"
    interfaces <- maybeImplements
    typeData <- outputObjectEntries
    pure (typeName, Implements interfaces $ createType typeName typeData)

maybeImplements :: Parser [Text]
maybeImplements = implements <|> pure []
  where
    implements =
      label "implements" $ do
        _ <- string "implements"
        space1
        spaceAndComments
        some token

dataInterface :: Parser (Text, RawDataType)
dataInterface =
  label "interface" $ do
    typeName <- typeDef "interface"
    typeData <- outputObjectEntries
    pure (typeName, Interface $ createType typeName typeData)

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

parseFinalDataType :: Parser (Text, DataFullType)
parseFinalDataType = label "dataType" $ dataInputObject <|> dataUnion <|> dataEnum <|> dataScalar

parseDataType :: Parser (Text, RawDataType)
parseDataType = label "dataType" $ dataInterface <|> dataObject <|> finalDataT
  where
    finalDataT = do
      (name, datatype) <- parseFinalDataType
      pure (name, FinalDataType datatype)
