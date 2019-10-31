module Data.Morpheus.Parsing.Internal.Pattern
  ( inputValueDefinition
  , fieldsDefinition
  , typDeclaration
  , optionalDirectives
  ) where


import           Text.Megaparsec                         (label, many)

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Create   (createField)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (keyword, optDescription, litAssignment, operator, parseAssignment,
                                                          parseMaybeTuple, parseName, parseType, setOf)
import           Data.Morpheus.Parsing.Internal.Value    (parseDefaultValue, parseValue)
import           Data.Morpheus.Types.Internal.Data       (DataField, Key, Name)


-- InputValue : https://graphql.github.io/graphql-spec/June2018/#InputValueDefinition
--
-- InputValueDefinition
--   Description(opt) Name : Type DefaultValue(opt) Directives (Const)(opt)
--
inputValueDefinition ::  Parser (Key, DataField)
inputValueDefinition = label "InputValueDefinition" $ do
    -- TODO: handle Description(opt)
    _description <- optDescription
    name <- parseName
    litAssignment -- ':'
    type_ <- parseType
    -- TODO: handle default value
    _ <- parseDefaultValue
    _ <- optionalDirectives
    pure (name, createField [] name type_)

-- Field Arguments: https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
--
-- ArgumentsDefinition:
--   ( InputValueDefinition(list) )
--
argumentsDefinition :: Parser [(Key, DataField)]
argumentsDefinition = label "ArgumentsDefinition" $  parseMaybeTuple inputValueDefinition


--  FieldsDefinition : https://graphql.github.io/graphql-spec/June2018/#FieldsDefinition
--
--  FieldsDefinition :
--    { FieldDefinition(list) }
--
fieldsDefinition :: Parser [(Key, DataField)]
fieldsDefinition = label "FieldsDefinition" $ setOf fieldDefinition


--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
fieldDefinition ::  Parser (Key, DataField)
fieldDefinition  = label "FieldDefinition" $ do
    -- TODO: handle Description(opt)
    _description <- optDescription
    name <- parseName
    args <- argumentsDefinition
    litAssignment -- ':'
    type_ <- parseType
    -- TODO: handle directives
    _ <- optionalDirectives
    pure (name, createField args name type_)

-- Directives : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Directives
--
-- example: @directive ( arg1: "value" , .... )
--
-- Directives[Const]
-- Directive[Const](list)
--
optionalDirectives :: Parser [()]
optionalDirectives = label "Directives" $ many directive

-- Directive[Const]
--
-- @ Name Arguments[Const](opt)
-- TODO:  returns real DataType
directive :: Parser ()
directive = label "Directive" $ do
    operator '@'
    _name <- parseName
    _args <- parseMaybeTuple (parseAssignment parseName parseValue)
    pure ()


-- typDeclaration : Not in spec ,start part of type definitions
--
--  typDeclaration
--   Description(opt) scalar Name
--
typDeclaration :: Name -> Parser Name
typDeclaration kind = do
  keyword kind
  parseName
