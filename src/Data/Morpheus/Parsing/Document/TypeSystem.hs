{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseDataType
  ) where

import           Data.Text                               (Text)
import           Text.Megaparsec                         (label, sepBy1, (<|>))

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Create   (createField)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Pattern  (fieldsDefinition, inputValueDefinition, optionalDirectives,
                                                          typDeclaration)
import           Data.Morpheus.Parsing.Internal.Terms    (keyword, operator, optDescription, parseName, pipeLiteral,
                                                          sepByAnd, setOf)
import           Data.Morpheus.Types.Internal.Data       (DataField, DataFingerprint (..), DataFullType (..),
                                                          DataLeaf (..), DataTyCon (..), DataValidator (..), Key,
                                                          RawDataType (..))


-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
scalarTypeDefinition :: Maybe Text -> Parser (Text, DataFullType)
scalarTypeDefinition typeDescription =
  label "ScalarTypeDefinition" $ do
    typeName <- typDeclaration "scalar"
    -- TODO: handle directives
    _ <- optionalDirectives
    pure (
          typeName,
          Leaf $ CustomScalar DataTyCon {
            typeName,
            typeDescription,
            typeFingerprint = SystemFingerprint typeName,
            typeData = DataValidator pure
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
objectTypeDefinition typeDescription =
  label "ObjectTypeDefinition" $ do
    name <- typDeclaration "type"
    interfaces <- optionalImplementsInterfaces
    -- TODO: handle directives
    _directives <- optionalDirectives
    fields <- fieldsDefinition
    --------------------------
    pure (name,
         Implements interfaces $ DataTyCon {
           typeName = name,
           typeDescription,
           typeFingerprint = SystemFingerprint name,
           typeData = fields
         }
      )

optionalImplementsInterfaces :: Parser [Text]
optionalImplementsInterfaces = implements <|> pure []
  where
    implements = label "ImplementsInterfaces" $ keyword "implements" *> sepByAnd parseName

-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
--
interfaceTypeDefinition :: Maybe Text -> Parser (Text, RawDataType)
interfaceTypeDefinition typeDescription =
  label "InterfaceTypeDefinition" $ do
    typeName <- typDeclaration "interface"
    -- TODO: handle directives
    _directives <- optionalDirectives
    fields <- fieldsDefinition
    pure (
        typeName,
        Interface DataTyCon {
           typeName,
           typeDescription,
           typeFingerprint = SystemFingerprint typeName,
           typeData = fields
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
unionTypeDefinition :: Maybe Text ->  Parser (Text, DataFullType)
unionTypeDefinition typeDescription =
  label "UnionTypeDefinition" $ do
    typeName <- typDeclaration "union"
    -- TODO: handle directives
    _directives <- optionalDirectives
    memberTypes <- unionMemberTypes
    pure (
        typeName,
        Union DataTyCon {
             typeName,
             typeDescription,
             typeFingerprint = SystemFingerprint typeName,
             typeData = map unionField memberTypes
           }
        )
  where
    unionField fieldType = createField [] "" ([], fieldType)
    unionMemberTypes = operator '=' *> parseName `sepBy1` pipeLiteral

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
enumTypeDefinition :: Maybe Text -> Parser (Text, DataFullType)
enumTypeDefinition typeDescription =
  label "EnumTypeDefinition" $ do
    typeName <- typDeclaration "enum"
    -- TODO: handle directives
    _directives <- optionalDirectives
    enumValuesDefinition <- setOf enumValueDefinition
    pure (
        typeName,
        Leaf $ LeafEnum DataTyCon {
             typeName,
             typeDescription,
             typeFingerprint = SystemFingerprint typeName,
             typeData = enumValuesDefinition
           }
        )
    where
        enumValueDefinition = do
            -- TODO: parse Description
            enumValueName <- parseName
            -- TODO: handle directives
            _directive <- optionalDirectives
            return enumValueName

-- Input Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Input-Objects
--
--   InputObjectTypeDefinition
--     Description(opt) input Name  Directives(Const)(opt) InputFieldsDefinition(opt)
--
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputObjectTypeDefinition :: Maybe Text -> Parser (Text, DataFullType)
inputObjectTypeDefinition typeDescription =
  label "InputObjectTypeDefinition" $ do
    typeName <- typDeclaration "input"
    -- TODO: handle directives
    _directives <- optionalDirectives
    fields <- inputFieldsDefinition
    pure
       (
         typeName,
         InputObject DataTyCon {
           typeName,
           typeDescription,
           typeFingerprint = SystemFingerprint typeName,
           typeData = fields
         }
       )
    where
      inputFieldsDefinition :: Parser [(Key, DataField)]
      inputFieldsDefinition = label "inputEntries" $ setOf inputValueDefinition


parseFinalDataType :: Maybe Text -> Parser (Text, DataFullType)
parseFinalDataType description = label "TypeDefinition" $
        inputObjectTypeDefinition description
    <|> unionTypeDefinition description
    <|> enumTypeDefinition description
    <|> scalarTypeDefinition description

parseDataType :: Parser (Text, RawDataType)
parseDataType = label "TypeDefinition" $ do
    description <- optDescription
    types description
    where
      types description = interfaceTypeDefinition description <|>
              objectTypeDefinition description <|>
              finalDataT
              where
                finalDataT = do
                  (name, datatype) <- parseFinalDataType description
                  pure (name, FinalDataType datatype)
