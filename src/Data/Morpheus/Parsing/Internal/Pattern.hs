{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Internal.Pattern
    ( inputValueDefinition
    , fieldsDefinition
    , typDeclaration
    , optionalDirectives
    , enumValueDefinition
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
import           Data.Morpheus.Types.Internal.AST
                                                ( DataField(..)
                                                , Directive(..)
                                                , Key
                                                , Meta(..)
                                                , DataEnumValue(..)
                                                , Name
                                                , TypeAlias(..)
                                                )


--  EnumValueDefinition: https://graphql.github.io/graphql-spec/June2018/#EnumValueDefinition
--
--  EnumValueDefinition
--    Description(opt) EnumValue Directives(Const)(opt)
--
enumValueDefinition :: Parser DataEnumValue
enumValueDefinition = label "EnumValueDefinition" $ do
    metaDescription <- optDescription
    enumName        <- parseName
    metaDirectives  <- optionalDirectives
    return $ DataEnumValue
        { enumName
        , enumMeta = Just Meta { metaDescription, metaDirectives }
        }

-- InputValue : https://graphql.github.io/graphql-spec/June2018/#InputValueDefinition
--
-- InputValueDefinition
--   Description(opt) Name : Type DefaultValue(opt) Directives (Const)(opt)
--
inputValueDefinition :: Parser (Key, DataField)
inputValueDefinition = label "InputValueDefinition" $ do
    metaDescription <- optDescription
    fieldName       <- parseName
    litAssignment -- ':'
    (aliasWrappers, aliasTyCon) <- parseType
    _                           <- parseDefaultValue
    metaDirectives              <- optionalDirectives
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
            , fieldMeta     = Just Meta { metaDescription, metaDirectives }
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
    metaDescription <- optDescription
    fieldName       <- parseName
    fieldArgs       <- argumentsDefinition
    litAssignment -- ':'
    (aliasWrappers, aliasTyCon) <- parseType
    metaDirectives              <- optionalDirectives
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
            , fieldMeta     = Just Meta { metaDescription, metaDirectives }
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
