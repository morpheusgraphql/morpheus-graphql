{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Internal.Pattern
    ( inputValueDefinition
    , fieldsDefinition
    , typDeclaration
    , optionalDirectives
    )
where


import           Text.Megaparsec                ( label
                                                , many
                                                )

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( keyword
                                                , litAssignment
                                                , operator
                                                , optDescription
                                                , parseAssignment
                                                , parseMaybeTuple
                                                , parseName
                                                , parseType
                                                , setOf
                                                )
import           Data.Morpheus.Parsing.Internal.Value
                                                ( parseDefaultValue
                                                , parseValue
                                                )
import           Data.Morpheus.Types.Internal.Data
                                                ( DataField(..)
                                                , Directive(..)
                                                , Key
                                                , Meta(..)
                                                , Name
                                                , TypeAlias(..)
                                                )


-- InputValue : https://graphql.github.io/graphql-spec/June2018/#InputValueDefinition
--
-- InputValueDefinition
--   Description(opt) Name : Type DefaultValue(opt) Directives (Const)(opt)
--
inputValueDefinition :: Parser (Key, DataField)
inputValueDefinition = label "InputValueDefinition" $ do
    -- TODO: handle Description(opt)
    metaDescription <- optDescription
    fieldName       <- parseName
    litAssignment -- ':'
    (aliasWrappers, aliasTyCon) <- parseType
    -- TODO: handle default value
    _                           <- parseDefaultValue
    _                           <- optionalDirectives
    pure
        ( fieldName
        , DataField
            { fieldArgs     = []
            , fieldArgsType = Nothing
            , fieldName
            , fieldType     = TypeAlias { aliasTyCon
                                        , aliasWrappers
                                        , aliasArgs     = Nothing
                                        }
            , fieldMeta     = Just Meta { metaDescription
                                        , metaDeprecated  = Nothing
                                        , metaDirectives  = []
                                        }
            }
        )

-- Field Arguments: https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
--
-- ArgumentsDefinition:
--   ( InputValueDefinition(list) )
--
argumentsDefinition :: Parser [(Key, DataField)]
argumentsDefinition =
    label "ArgumentsDefinition" $ parseMaybeTuple inputValueDefinition


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
fieldDefinition :: Parser (Key, DataField)
fieldDefinition = label "FieldDefinition" $ do
    -- TODO: handle Description(opt)
    metaDescription <- optDescription
    fieldName       <- parseName
    fieldArgs       <- argumentsDefinition
    litAssignment -- ':'
    (aliasWrappers, aliasTyCon) <- parseType
    -- TODO: handle directives
    _                           <- optionalDirectives
    pure
        ( fieldName
        , DataField
            { fieldName
            , fieldArgs
            , fieldArgsType = Nothing
            , fieldType     = TypeAlias { aliasTyCon
                                        , aliasWrappers
                                        , aliasArgs     = Nothing
                                        }
            , fieldMeta     = Just Meta { metaDescription
                                        , metaDeprecated  = Nothing
                                        , metaDirectives  = []
                                        }
            }
        )

-- Directives : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Directives
--
-- example: @directive ( arg1: "value" , .... )
--
-- Directives[Const]
-- Directive[Const](list)
--
optionalDirectives :: Parser [Directive]
optionalDirectives = label "Directives" $ many directive

-- Directive[Const]
--
-- @ Name Arguments[Const](opt)
-- TODO:  returns real DataType
directive :: Parser Directive
directive = label "Directive" $ do
    operator '@'
    directiveName <- parseName
    directiveArgs <- parseMaybeTuple (parseAssignment parseName parseValue)
    pure Directive { directiveName, directiveArgs }


-- typDeclaration : Not in spec ,start part of type definitions
--
--  typDeclaration
--   Description(opt) scalar Name
--
typDeclaration :: Name -> Parser Name
typDeclaration kind = do
    keyword kind
    parseName
