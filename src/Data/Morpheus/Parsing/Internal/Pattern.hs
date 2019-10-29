module Data.Morpheus.Parsing.Internal.Pattern
  ( directive
  , inputValueDefinition
  , fieldsDefinition
  , typDeclaration
  ) where


import           Text.Megaparsec                         (label, optional)
import           Text.Megaparsec.Char                    (char)

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Create   (createField)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (keyword, litAssignment, parseAssignment, parseMaybeTuple,
                                                          parseName, parseTuple, parseType, qualifier, operator, setOf)
import           Data.Morpheus.Parsing.Internal.Value    (parseDefaultValue, parseValue)
import           Data.Morpheus.Types.Internal.Data       (DataField, Key, Name)


-- InputValue : https://graphql.github.io/graphql-spec/June2018/#InputValueDefinition
--
-- InputValueDefinition
--   Description(opt) Name : Type DefaultValue(opt) Directives (Const)(opt)
--
inputValueDefinition ::  Parser (Key, DataField)
inputValueDefinition = label "InputValueDefinition" $ do
    -- TODO: Description(opt)
    name <- parseName
    litAssignment -- ':'
    type_ <- parseType
    -- TODO: handle default value
    _ <- parseDefaultValue
    _ <- optional directive
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
    -- TODO: Description(opt)
    name <- parseName
    args <- argumentsDefinition
    litAssignment -- ':'
    type_ <- parseType
    _ <- optional directive
    pure (name, createField args name type_)

-- @directive ( arg1: "value" , .... )
-- TODO:  returns real DataType
directive :: Parser ()
directive = do
    _ <- char '@'
    _name <- qualifier
    _boo <- parseTuple (parseAssignment qualifier parseValue) -- TODO: string
    pure ()

-- typDeclaration : Not in spec ,start part of type definitions
--
--  typDeclaration
--   Description(opt) scalar Name
--
typDeclaration :: Name -> Parser Name
typDeclaration kind = do
  -- TODO: Description(opt)
  keyword kind
  parseName
