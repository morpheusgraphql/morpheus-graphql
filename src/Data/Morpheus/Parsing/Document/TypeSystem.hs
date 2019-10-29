{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseDataType
  ) where

import           Data.Morpheus.Parsing.Internal.Create   (createArgument, createEnumType, createField, createScalarType,
                                                          createType, createUnionType)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Pattern  (directive, inputValueDefinition)
import           Data.Morpheus.Parsing.Internal.Terms    (parseAssignment, parseMaybeTuple, parseNonNull,
                                                          parseWrappedType, pipeLiteral, qualifier, sepByAnd, setOf,
                                                          spaceAndComments, token)
import           Data.Morpheus.Parsing.Internal.Value    (parseDefaultValue)
import           Data.Morpheus.Types.Internal.Data       (DataField, DataFullType (..), Key, RawDataType (..),
                                                          toHSWrappers)
import           Data.Text                               (Text)
import           Text.Megaparsec                         (label, optional, sepBy1, (<|>))
import           Text.Megaparsec.Char                    (char, space1, string)

-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
dataScalar :: Parser (Text, DataFullType)
dataScalar =
  label "scalar" $ do
    typeName <- typeDef "scalar"
    pure $ createScalarType typeName


-- Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Objects
--
--  ObjectTypeDefinition:
--    Description(opt) type Name ImplementsInterfaces(opt) Directives(Const)(opt) FieldsDefinition(opt)
--
--  ImplementsInterfaces
--    implements &(opt) NamedType
--    ImplementsInterfaces & NamedType
--
--  FieldsDefinition
--    { FieldDefinition(list) }
--
--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
dataObject :: Parser (Text, RawDataType)
dataObject =
  label "object" $ do
    typeName <- typeDef "type"
    interfaces <- maybeImplements
    typeData <- outputObjectEntries
    pure (typeName, Implements interfaces $ createType typeName typeData)

-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
-- 



-- Unions : https://graphql.github.io/graphql-spec/June2018/#sec-Unions
--
--  UnionTypeDefinition:
--    Description(opt) union Name Directives(Const)(opt) UnionMemberTypes(opt)
--
--  UnionMemberTypes:
--    = |(opt) NamedType
--      UnionMemberTypes | NamedType
-- 


-- Enums : https://graphql.github.io/graphql-spec/June2018/#sec-Enums
--
--  EnumTypeDefinition
--    Description(opt) enum Name Directives(Const)(opt) EnumValuesDefinition(opt)
--
--  EnumValuesDefinition
--    { EnumValueDefinition(list) }
--
--  EnumValueDefinition
--    Description(opt) EnumValue Directives(Const)(opt)
--



-- Input Objects: https://graphql.github.io/graphql-spec/June2018/#sec-Input-Objects
--
--   InputObjectTypeDefinition
--     Description(opt) input Name  Directives(Const)(opt) InputFieldsDefinition(opt)
--
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputObjectTypeDefinition :: Parser (Text, DataFullType)
inputObjectTypeDefinition =
  label "inputObject" $ do
    typeName <- typeDef "input"
    typeData <- inputFieldsDefinition
    pure (typeName, InputObject $ createType typeName typeData)
    where
      inputFieldsDefinition :: Parser [(Key, DataField)]
      inputFieldsDefinition = label "inputEntries" $ setOf inputValueDefinition

typeDef :: Text -> Parser Text
typeDef kind = do
  _ <- string kind
  space1
  token

entryWith :: Parser [(Text, DataField)] -> Parser (Key, DataField)
entryWith argsParser = label "entry" $ do
    ((fieldName, fieldArgs), (wrappers, fieldType)) <- parseAssignment fieldWithArgs parseWrappedType
    nonNull <- parseNonNull
    _ <- optional directive
    return (fieldName, createField fieldArgs fieldName (toHSWrappers $ nonNull ++ wrappers, fieldType))
    where
        fieldWithArgs =
          label "fieldWithArgs" $ do
            (name, _) <- qualifier
            args <- argsParser
            return (name, args)



outputObjectEntries :: Parser [(Key, DataField)]
outputObjectEntries = label "entries" $ setOf (entryWith (parseMaybeTuple dataArgument))



maybeImplements :: Parser [Text]
maybeImplements = implements <|> pure []
  where
    implements =
      label "implements" $ do
        _ <- string "implements"
        space1
        spaceAndComments
        sepByAnd token

dataInterface :: Parser (Text, RawDataType)
dataInterface =
  label "interface" $ do
    typeName <- typeDef "interface"
    typeData <- outputObjectEntries
    pure (typeName, Interface $ createType typeName typeData)



dataEnum :: Parser (Text, DataFullType)
dataEnum =
  label "enum" $ do
    typeName <- typeDef "enum"
    typeData <- setOf ( token <* optional directive)
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
parseFinalDataType = label "dataType" $ inputObjectTypeDefinition <|> dataUnion <|> dataEnum <|> dataScalar

parseDataType :: Parser (Text, RawDataType)
parseDataType = label "dataType" $ dataInterface <|> dataObject <|> finalDataT
  where
    finalDataT = do
      (name, datatype) <- parseFinalDataType
      pure (name, FinalDataType datatype)

dataArgument :: Parser (Text, DataField)
dataArgument =
  label "Argument" $ do
    ((fieldName, _), (wrappers, fieldType)) <- parseAssignment qualifier parseWrappedType
    nonNull <- parseNonNull
    -- TODO: handle default value
    defaultValue <- parseDefaultValue
    pure $ createArgument fieldName (toHSWrappers $ nonNull ++ wrappers, fieldType)
