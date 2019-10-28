module Data.Morpheus.Parsing.Internal.Pattern 
  ( directive 
  , inputValueDefinition 
  ) where


import           Text.Megaparsec                         (label, optional)
import           Text.Megaparsec.Char                    (char)

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Create   (createField)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (parseAssignment, parseNonNull, parseTuple, parseWrappedType,
                                                          qualifier)
import           Data.Morpheus.Parsing.Internal.Value    (parseDefaultValue, parseValue)
import           Data.Morpheus.Types.Internal.Data       (DataField, Key, WrapperD, toHSWrappers)


typeDefinition :: Parser ([WrapperD],Key)
typeDefinition = do
    (wrappers, fieldType) <- parseWrappedType
    nonNull <- parseNonNull
    return (toHSWrappers $ nonNull ++ wrappers, fieldType)

-- InputValueDefinition:
--
--   Description(opt) Name : Type DefaultValue(opt) Directives (Const)(opt)
--   {
--      url: https://graphql.github.io/graphql-spec/June2018/#InputValueDefinition
--      usedIn: ArgumentsDefinition , InputObjectTypeDefinition
--   }
inputValueDefinition ::  Parser (Key, DataField)
inputValueDefinition = label "entry" $ do
    ((fieldName, _), (wrappers, fieldType)) <- parseAssignment qualifier typeDefinition
    -- TODO: handle default value
    defaultValue <- parseDefaultValue
    _ <- optional directive
    return (fieldName, createField [] fieldName (wrappers, fieldType))


-- @directive ( arg1: "value" , .... )
-- TODO:  returns real DataType
directive :: Parser ()
directive = do
    _ <- char '@'
    _name <- qualifier
    _boo <- parseTuple (parseAssignment qualifier parseValue) -- TODO: string
    pure ()
