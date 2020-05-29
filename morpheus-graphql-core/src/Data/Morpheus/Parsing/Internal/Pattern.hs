{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Parsing.Internal.Pattern
  ( inputValueDefinition,
    fieldsDefinition,
    typeDeclaration,
    optionalDirectives,
    enumValueDefinition,
    inputFieldsDefinition,
  )
where

-- MORPHEUS

import Data.Morpheus.Parsing.Internal.Arguments
  ( maybeArguments,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( keyword,
    litAssignment,
    operator,
    optDescription,
    parseName,
    parseType,
    parseTypeName,
    setOf,
    uniqTuple,
  )
import Data.Morpheus.Parsing.Internal.Value
  ( Parse (..),
    parseDefaultValue,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    DataEnumValue (..),
    Directive (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    IN,
    InputFieldsDefinition,
    OUT,
    TypeName,
    Value,
  )
import Text.Megaparsec
  ( (<|>),
    label,
    many,
  )

--  EnumValueDefinition: https://graphql.github.io/graphql-spec/June2018/#EnumValueDefinition
--
--  EnumValueDefinition
--    Description(opt) EnumValue Directives(Const)(opt)
--
enumValueDefinition :: Parser DataEnumValue
enumValueDefinition = label "EnumValueDefinition" $ do
  enumDescription <- optDescription
  enumName <- parseTypeName
  enumDirectives <- optionalDirectives
  return DataEnumValue {..}

-- InputValue : https://graphql.github.io/graphql-spec/June2018/#InputValueDefinition
--
-- InputValueDefinition
--   Description(opt) Name : Type DefaultValue(opt) Directives (Const)(opt)
--
inputValueDefinition :: Parser (FieldDefinition IN)
inputValueDefinition = label "InputValueDefinition" $ do
  fieldDescription <- optDescription
  fieldName <- parseName
  litAssignment -- ':'
  fieldType <- parseType
  fieldContent <- DefaultInputValue <$> parseDefaultValue
  fieldDirectives <- optionalDirectives
  pure FieldDefinition {..}

-- Field Arguments: https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
--
-- ArgumentsDefinition:
--   ( InputValueDefinition(list) )
--
argumentsDefinition :: Parser ArgumentsDefinition
argumentsDefinition =
  label "ArgumentsDefinition" $
    uniqTuple inputValueDefinition
      <|> pure NoArguments

--  FieldsDefinition : https://graphql.github.io/graphql-spec/June2018/#FieldsDefinition
--
--  FieldsDefinition :
--    { FieldDefinition(list) }
--
fieldsDefinition :: Parser (FieldsDefinition OUT)
fieldsDefinition = label "FieldsDefinition" $ setOf fieldDefinition

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
fieldDefinition :: Parser (FieldDefinition OUT)
fieldDefinition = label "FieldDefinition" $ do
  fieldDescription <- optDescription
  fieldName <- parseName
  fieldContent <- FieldArgs <$> argumentsDefinition
  litAssignment -- ':'
  fieldType <- parseType
  fieldDirectives <- optionalDirectives
  pure FieldDefinition {..}

-- InputFieldsDefinition : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Directives
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputFieldsDefinition :: Parser InputFieldsDefinition
inputFieldsDefinition = label "InputFieldsDefinition" $ setOf inputValueDefinition

-- Directives : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Directives
--
-- example: @directive ( arg1: "value" , .... )
--
-- Directives[Const]
-- Directive[Const](list)
--
optionalDirectives :: Parse (Value s) => Parser [Directive s]
optionalDirectives = label "Directives" $ many directive

-- Directive[Const]
--
-- @ Name Arguments[Const](opt)
directive :: Parse (Value s) => Parser (Directive s)
directive = label "Directive" $ do
  directivePosition <- getLocation
  operator '@'
  directiveName <- parseName
  directiveArgs <- maybeArguments
  pure Directive {..}

-- typDeclaration : Not in spec ,start part of type definitions
--
--  typDeclaration
--   Description(opt) scalar Name
--
typeDeclaration :: FieldName -> Parser TypeName
typeDeclaration kind = do
  keyword kind
  parseTypeName
