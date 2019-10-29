module Data.Morpheus.Parsing.Internal.Pattern
  ( directive
  , inputValueDefinition
  , fieldsDefinition
  ) where


import           Text.Megaparsec                         (label, optional)
import           Text.Megaparsec.Char                    (char)

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Create   (createField)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (litAssignment, parseAssignment, parseMaybeTuple, parseName,
                                                          parseNonNull, parseTuple, parseWrappedType, qualifier, setOf)
import           Data.Morpheus.Parsing.Internal.Value    (parseDefaultValue, parseValue)
import           Data.Morpheus.Types.Internal.Data       (DataField, Key, WrapperD, toHSWrappers)


typeDefinition :: Parser ([WrapperD],Key)
typeDefinition = do
    (wrappers, fieldType) <- parseWrappedType
    nonNull <- parseNonNull
    pure (toHSWrappers $ nonNull ++ wrappers, fieldType)

-- InputValue : https://graphql.github.io/graphql-spec/June2018/#InputValueDefinition
--
-- InputValueDefinition
--   Description(opt) Name : Type DefaultValue(opt) Directives (Const)(opt)
--
inputValueDefinition ::  Parser (Key, DataField)
inputValueDefinition = label "InputValueDefinition" $ do
    -- TODO: Description(opt)
    name <- parseName
    litAssignment -- parser ':'
    typeRef <- typeDefinition
    -- TODO: handle default value
    _ <- parseDefaultValue
    _ <- optional directive
    pure (name, createField [] name typeRef)

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
fieldsDefinition = label "fieldsDefinition" $ setOf fieldDefinition


--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
fieldDefinition ::  Parser (Key, DataField)
fieldDefinition  = label "fieldDefinition" $ do
    -- TODO: Description(opt)
    name <- parseName
    args <- argumentsDefinition
    litAssignment -- parser ':'
    typeDef <- typeDefinition
    _ <- optional directive
    pure (name, createField args name typeDef)

-- @directive ( arg1: "value" , .... )
-- TODO:  returns real DataType
directive :: Parser ()
directive = do
    _ <- char '@'
    _name <- qualifier
    _boo <- parseTuple (parseAssignment qualifier parseValue) -- TODO: string
    pure ()
