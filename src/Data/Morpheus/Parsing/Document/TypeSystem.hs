{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseDataType
  )
where

import           Data.Text                      ( Text )
import           Text.Megaparsec                ( label
                                                , sepBy1
                                                , (<|>)
                                                )

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser )
import           Data.Morpheus.Parsing.Internal.Pattern
                                                ( fieldsDefinition
                                                , inputValueDefinition
                                                , optionalDirectives
                                                , typDeclaration
                                                , enumValueDefinition
                                                )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( keyword
                                                , operator
                                                , optDescription
                                                , parseName
                                                , pipeLiteral
                                                , sepByAnd
                                                , setOf
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataField
                                                , DataFingerprint(..)
                                                , DataTyCon(..)
                                                , DataType(..)
                                                , DataValidator(..)
                                                , Key
                                                , RawDataType(..)
                                                , Meta(..)
                                                )


-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
scalarTypeDefinition :: Maybe Text -> Parser (Text, DataType)
scalarTypeDefinition metaDescription = label "ScalarTypeDefinition" $ do
  typeName       <- typDeclaration "scalar"
  metaDirectives <- optionalDirectives
  pure
    ( typeName
    , DataScalar DataTyCon
      { typeName
      , typeMeta        = Just Meta { metaDescription, metaDirectives }
      , typeFingerprint = SystemFingerprint typeName
      , typeData        = DataValidator pure
      }
    )

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
objectTypeDefinition :: Maybe Text -> Parser (Text, RawDataType)
objectTypeDefinition metaDescription = label "ObjectTypeDefinition" $ do
  name           <- typDeclaration "type"
  interfaces     <- optionalImplementsInterfaces
  metaDirectives <- optionalDirectives
  fields         <- fieldsDefinition
  --------------------------
  pure
    ( name
    , Implements interfaces $ DataTyCon
      { typeName        = name
      , typeMeta        = Just Meta { metaDescription, metaDirectives }
      , typeFingerprint = SystemFingerprint name
      , typeData        = fields
      }
    )

optionalImplementsInterfaces :: Parser [Text]
optionalImplementsInterfaces = implements <|> pure []
 where
  implements =
    label "ImplementsInterfaces" $ keyword "implements" *> sepByAnd parseName

-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
--
interfaceTypeDefinition :: Maybe Text -> Parser (Text, RawDataType)
interfaceTypeDefinition metaDescription = label "InterfaceTypeDefinition" $ do
  typeName       <- typDeclaration "interface"
  metaDirectives <- optionalDirectives
  fields         <- fieldsDefinition
  pure
    ( typeName
    , Interface DataTyCon
      { typeName
      , typeMeta        = Just Meta { metaDescription, metaDirectives }
      , typeFingerprint = SystemFingerprint typeName
      , typeData        = fields
      }
    )


-- Unions : https://graphql.github.io/graphql-spec/June2018/#sec-Unions
--
--  UnionTypeDefinition:
--    Description(opt) union Name Directives(Const)(opt) UnionMemberTypes(opt)
--
--  UnionMemberTypes:
--    = |(opt) NamedType
--      UnionMemberTypes | NamedType
--
unionTypeDefinition :: Maybe Text -> Parser (Text, DataType)
unionTypeDefinition metaDescription = label "UnionTypeDefinition" $ do
  typeName       <- typDeclaration "union"
  metaDirectives <- optionalDirectives
  memberTypes    <- unionMemberTypes
  pure
    ( typeName
    , DataUnion DataTyCon
      { typeName
      , typeMeta        = Just Meta { metaDescription, metaDirectives }
      , typeFingerprint = SystemFingerprint typeName
      , typeData        = memberTypes
      }
    )
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
enumTypeDefinition :: Maybe Text -> Parser (Text, DataType)
enumTypeDefinition metaDescription = label "EnumTypeDefinition" $ do
  typeName              <- typDeclaration "enum"
  metaDirectives        <- optionalDirectives
  enumValuesDefinitions <- setOf enumValueDefinition
  pure
    ( typeName
    , DataEnum DataTyCon
      { typeName
      , typeMeta        = Just Meta { metaDescription, metaDirectives }
      , typeFingerprint = SystemFingerprint typeName
      , typeData        = enumValuesDefinitions
      }
    )


-- Input Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Input-Objects
--
--   InputObjectTypeDefinition
--     Description(opt) input Name  Directives(Const)(opt) InputFieldsDefinition(opt)
--
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputObjectTypeDefinition :: Maybe Text -> Parser (Text, DataType)
inputObjectTypeDefinition metaDescription =
  label "InputObjectTypeDefinition" $ do
    typeName       <- typDeclaration "input"
    metaDirectives <- optionalDirectives
    fields         <- inputFieldsDefinition
    pure
      ( typeName
      , DataInputObject DataTyCon
        { typeName
        , typeMeta        = Just Meta { metaDescription, metaDirectives }
        , typeFingerprint = SystemFingerprint typeName
        , typeData        = fields
        }
      )
 where
  inputFieldsDefinition :: Parser [(Key, DataField)]
  inputFieldsDefinition =
    label "InputFieldsDefinition" $ setOf inputValueDefinition


parseFinalDataType :: Maybe Text -> Parser (Text, DataType)
parseFinalDataType description =
  label "TypeDefinition"
    $   inputObjectTypeDefinition description
    <|> unionTypeDefinition description
    <|> enumTypeDefinition description
    <|> scalarTypeDefinition description

parseDataType :: Parser (Text, RawDataType)
parseDataType = label "TypeDefinition" $ do
  description <- optDescription
  types description
 where
  types description =
    interfaceTypeDefinition description
      <|> objectTypeDefinition description
      <|> finalDataT
   where
    finalDataT = do
      (name, datatype) <- parseFinalDataType description
      pure (name, FinalDataType datatype)
