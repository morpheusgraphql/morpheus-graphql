{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Internal.Pattern
    ( inputValueDefinition
    , fieldsDefinition
    , typDeclaration
    , optionalDirectives
    , enumValueDefinition
    , inputFieldsDefinition
    )
where

import           Text.Megaparsec                ( label
                                                , many
                                                , (<|>)
                                                )

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( keyword
                                                , litAssignment
                                                , operator
                                                , optDescription
                                                , parseName
                                                , parseType
                                                , setOf
                                                , uniqTuple
                                                )
import           Data.Morpheus.Parsing.Internal.Arguments
                                                ( parseArgumentsOpt )
import           Data.Morpheus.Parsing.Internal.Value
                                                ( parseDefaultValue )
import           Data.Morpheus.Types.Internal.AST
                                                ( FieldDefinition(..)
                                                , Directive(..)
                                                , Meta(..)
                                                , DataEnumValue(..)
                                                , Name
                                                , ArgumentsDefinition(..)
                                                , FieldsDefinition(..)
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
inputValueDefinition :: Parser FieldDefinition
inputValueDefinition = label "InputValueDefinition" $ do
    metaDescription <- optDescription
    fieldName       <- parseName
    litAssignment -- ':'
    fieldType      <- parseType
    _              <- parseDefaultValue
    metaDirectives <- optionalDirectives
    pure FieldDefinition 
        { fieldArgs     = NoArguments
        , fieldName
        , fieldType
        , fieldMeta = Just Meta { metaDescription, metaDirectives }
        }

-- Field Arguments: https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
--
-- ArgumentsDefinition:
--   ( InputValueDefinition(list) )
--
argumentsDefinition :: Parser ArgumentsDefinition
argumentsDefinition = label "ArgumentsDefinition" 
     $  uniqTuple inputValueDefinition
    <|> pure NoArguments

--  FieldsDefinition : https://graphql.github.io/graphql-spec/June2018/#FieldsDefinition
--
--  FieldsDefinition :
--    { FieldDefinition(list) }
--
fieldsDefinition :: Parser FieldsDefinition
fieldsDefinition = label "FieldsDefinition" $ setOf fieldDefinition

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
fieldDefinition :: Parser FieldDefinition
fieldDefinition = label "FieldDefinition" $ do
    metaDescription <- optDescription
    fieldName       <- parseName
    fieldArgs       <- argumentsDefinition
    litAssignment -- ':'
    fieldType      <- parseType
    metaDirectives <- optionalDirectives
    pure FieldDefinition 
        { fieldName
        , fieldArgs
        , fieldType
        , fieldMeta = Just Meta { metaDescription, metaDirectives }
        }
        
-- InputFieldsDefinition : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Directives
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputFieldsDefinition :: Parser FieldsDefinition
inputFieldsDefinition = label "InputFieldsDefinition" $ setOf inputValueDefinition

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
    directiveArgs <- parseArgumentsOpt
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
