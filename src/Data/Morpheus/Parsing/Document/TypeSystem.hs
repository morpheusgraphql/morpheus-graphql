{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseSchema
  )
where

import           Data.Text                      ( Text )
import           Text.Megaparsec                ( label
                                                , sepBy1
                                                , (<|>)
                                                , eof
                                                , manyTill
                                                , runParser
                                                )

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser
                                                , processErrorBundle
                                                )
import           Data.Morpheus.Parsing.Internal.Pattern
                                                ( fieldsDefinition
                                                , optionalDirectives
                                                , typDeclaration
                                                , enumValueDefinition
                                                , inputFieldsDefinition
                                                )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( keyword
                                                , operator
                                                , optDescription
                                                , parseName
                                                , pipeLiteral
                                                , sepByAnd
                                                , setOf
                                                , spaceAndComments
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataFingerprint(..)
                                                , DataTypeContent(..)
                                                , DataType(..)
                                                , Name
                                                , Description
                                                , Meta(..)
                                                , ScalarDefinition(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                 ( Validation
                                                 , Failure(..)
                                                 )

-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
scalarTypeDefinition :: Maybe Description -> Parser DataType
scalarTypeDefinition metaDescription = label "ScalarTypeDefinition" $ do
  typeName       <- typDeclaration "scalar"
  metaDirectives <- optionalDirectives
  pure DataType 
    { typeName
    , typeMeta        = Just Meta { metaDescription, metaDirectives }
    , typeFingerprint = DataFingerprint typeName []
    , typeContent     = DataScalar $ ScalarDefinition pure
    }

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
objectTypeDefinition :: Maybe Description -> Parser DataType
objectTypeDefinition metaDescription = label "ObjectTypeDefinition" $ do
  typeName         <- typDeclaration "type"
  objectImplements <- optionalImplementsInterfaces
  metaDirectives   <- optionalDirectives
  objectFields     <- fieldsDefinition
  -- build object
  pure DataType
    { typeName
    , typeMeta          = Just Meta { metaDescription, metaDirectives }
    , typeFingerprint   = DataFingerprint typeName []
    , typeContent       = DataObject { objectImplements, objectFields }
    }

optionalImplementsInterfaces :: Parser [Name]
optionalImplementsInterfaces = implements <|> pure []
 where
  implements =
    label "ImplementsInterfaces" $ keyword "implements" *> sepByAnd parseName

-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
--
interfaceTypeDefinition :: Maybe Description -> Parser DataType
interfaceTypeDefinition metaDescription = label "InterfaceTypeDefinition" $ do
  typeName  <- typDeclaration "interface"
  metaDirectives <- optionalDirectives
  fields         <- fieldsDefinition
  -- build interface
  pure DataType 
    { typeName
    , typeMeta        = Just Meta { metaDescription, metaDirectives }
    , typeFingerprint = DataFingerprint typeName []
    , typeContent     = DataInterface fields
    }

-- Unions : https://graphql.github.io/graphql-spec/June2018/#sec-Unions
--
--  UnionTypeDefinition:
--    Description(opt) union Name Directives(Const)(opt) UnionMemberTypes(opt)
--
--  UnionMemberTypes:
--    = |(opt) NamedType
--      UnionMemberTypes | NamedType
--
unionTypeDefinition :: Maybe Description -> Parser DataType
unionTypeDefinition metaDescription = label "UnionTypeDefinition" $ do
  typeName       <- typDeclaration "union"
  metaDirectives <- optionalDirectives
  memberTypes    <- unionMemberTypes
  -- build union
  pure DataType 
    { typeName
    , typeMeta        = Just Meta { metaDescription, metaDirectives }
    , typeFingerprint = DataFingerprint typeName []
    , typeContent     = DataUnion memberTypes
    }
  where unionMemberTypes = operator '=' *> parseName `sepBy1` pipeLiteral

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
enumTypeDefinition :: Maybe Description -> Parser DataType
enumTypeDefinition metaDescription = label "EnumTypeDefinition" $ do
  typeName              <- typDeclaration "enum"
  metaDirectives        <- optionalDirectives
  enumValuesDefinitions <- setOf enumValueDefinition
  -- build enum
  pure DataType 
    { typeName
    , typeContent     = DataEnum enumValuesDefinitions
    , typeFingerprint = DataFingerprint typeName []
    , typeMeta        = Just Meta { metaDescription, metaDirectives }
    }

-- Input Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Input-Objects
--
--   InputObjectTypeDefinition
--     Description(opt) input Name  Directives(Const)(opt) InputFieldsDefinition(opt)
--
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputObjectTypeDefinition :: Maybe Description -> Parser DataType
inputObjectTypeDefinition metaDescription =
  label "InputObjectTypeDefinition" $ do
    typeName       <- typDeclaration "input"
    metaDirectives <- optionalDirectives
    fields         <- inputFieldsDefinition
    -- build input
    pure DataType 
      { typeName
      , typeContent     = DataInputObject fields
      , typeFingerprint = DataFingerprint typeName []
      , typeMeta = Just Meta { metaDescription, metaDirectives }
      }

parseDataType :: Parser DataType
parseDataType = label "TypeDefinition" $ do  
  description <- optDescription
  -- scalar | enum |  input | object | union | interface
  inputObjectTypeDefinition description
      <|> unionTypeDefinition description
      <|> enumTypeDefinition description
      <|> scalarTypeDefinition description
      <|> objectTypeDefinition description
      <|> interfaceTypeDefinition description

parseSchema :: Text -> Validation [DataType]
parseSchema doc = case parseDoc of
  Right root       -> pure root
  Left  parseError -> failure (processErrorBundle parseError)
 where
  parseDoc = runParser request "<input>" doc
  request  = label "DocumentTypes" $ do
    spaceAndComments
    manyTill parseDataType eof