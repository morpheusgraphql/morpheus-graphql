{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.Parsing.DataType
  ( parseDataType
  ) where

import           Data.Morpheus.Document.Parsing.Terms (Parser, nonNull, parseAssignment, parseMaybeTuple, pipe,
                                                       qualifier, setOf, token, wrappedType)
import           Data.Morpheus.Types.Internal.Data    (DataArgument, DataField (..), DataFingerprint (..),
                                                       DataFullType (..), DataLeaf (..), DataOutputField, DataType (..),
                                                       DataTypeKind (..), Key)
import           Data.Text                            (Text)
import           Text.Megaparsec                      (label, sepBy1, (<|>))
import           Text.Megaparsec.Char                 (char, space, space1, string)

dataArgument :: Parser (Text, DataArgument)
dataArgument =
  label "Argument" $ do
    ((fieldName, _), (wrappers', fieldType)) <- parseAssignment qualifier wrappedType
    nonNull' <- nonNull
    pure
      ( fieldName
      , DataField
          { fieldArgs = ()
          , fieldName
          , fieldKind = KindObject -- TODO : realKinds
          , fieldType
          , fieldTypeWrappers = nonNull' ++ wrappers'
          , fieldHidden = False
          })

entries :: Parser [(Key, DataOutputField)]
entries = label "entries" $ setOf entry
  where
    fieldWithArgs =
      label "fieldWithArgs" $ do
        (name, _) <- qualifier
        args <- parseMaybeTuple dataArgument
        return (name, args)
    entry =
      label "entry" $ do
        ((fieldName, fieldArgs), (wrappers', fieldType)) <- parseAssignment fieldWithArgs wrappedType
        nonNull' <- nonNull
        -- variables <- parseMaybeTuple dataArgument
        return
          ( fieldName
          , DataField
              { fieldArgs
              , fieldName
              , fieldKind = KindObject -- TODO : realKinds
              , fieldType
              , fieldTypeWrappers = nonNull' ++ wrappers'
              , fieldHidden = False
              })

typeDef :: Text -> Parser Text
typeDef kind = do
  _ <- string kind
  space1
  token

dataObject :: Parser (Text, DataFullType)
dataObject =
  label "dataObject" $ do
    typeName <- typeDef "type"
    typeData <- entries
    pure
      ( typeName
      , OutputObject $
        DataType
          {typeName, typeDescription = "", typeFingerprint = SystemFingerprint "", typeVisibility = True, typeData})

dataEnum :: Parser (Text, DataFullType)
dataEnum =
  label "dataObject" $ do
    typeName <- typeDef "enum"
    typeData <- setOf token
    pure
      ( typeName
      , Leaf $
        LeafEnum $
        DataType
          {typeName, typeDescription = "", typeFingerprint = SystemFingerprint "", typeVisibility = True, typeData})

dataUnion :: Parser (Text, DataFullType)
dataUnion =
  label "dataUnion" $ do
    typeName <- typeDef "union"
    _ <- char '='
    space
    typeData <- map unionField <$> unionsParser
    space
    pure
      ( typeName
      , Union $
        DataType
          {typeName, typeDescription = "", typeFingerprint = SystemFingerprint "", typeVisibility = True, typeData})
  where
    unionsParser = token `sepBy1` pipe
    unionField name =
      DataField
        { fieldArgs = ()
        , fieldName = ""
        , fieldKind = KindObject
        , fieldType = name
        , fieldTypeWrappers = []
        , fieldHidden = False
        }

parseDataType :: Parser (Text, DataFullType)
parseDataType = label "dataType" $ dataObject <|> dataUnion <|> dataEnum
