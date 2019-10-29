module Data.Morpheus.Parsing.Internal.Pattern
  ( directive
  , inputValueDefinition
  , fieldsDefinition
  ) where


import           Text.Megaparsec                         (label, optional)
import           Text.Megaparsec.Char                    (char)

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Create   (createArgument, createField)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (parseAssignment, parseMaybeTuple, parseNonNull, parseTuple,
                                                          parseWrappedType, qualifier, setOf)
import           Data.Morpheus.Parsing.Internal.Value    (parseDefaultValue, parseValue)
import           Data.Morpheus.Types.Internal.Data       (DataField, Key, WrapperD, toHSWrappers)


typeDefinition :: Parser ([WrapperD],Key)
typeDefinition = do
    (wrappers, fieldType) <- parseWrappedType
    nonNull <- parseNonNull
    return (toHSWrappers $ nonNull ++ wrappers, fieldType)

-- InputValue : https://graphql.github.io/graphql-spec/June2018/#InputValueDefinition
--
-- InputValueDefinition
--   Description(opt) Name : Type DefaultValue(opt) Directives (Const)(opt)
--
inputValueDefinition ::  Parser (Key, DataField)
inputValueDefinition = label "entry" $ do
    ((fieldName, _), (wrappers, fieldType)) <- parseAssignment qualifier typeDefinition
    -- TODO: handle default value
    defaultValue <- parseDefaultValue
    _ <- optional directive
    return (fieldName, createField [] fieldName (wrappers, fieldType))

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
--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
fieldsDefinition :: Parser [(Key, DataField)]
fieldsDefinition = label "entries" $ setOf (entryWith argumentsDefinition)
    where
        entryWith :: Parser [(Key, DataField)] -> Parser (Key, DataField)
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

-- @directive ( arg1: "value" , .... )
-- TODO:  returns real DataType
directive :: Parser ()
directive = do
    _ <- char '@'
    _name <- qualifier
    _boo <- parseTuple (parseAssignment qualifier parseValue) -- TODO: string
    pure ()
