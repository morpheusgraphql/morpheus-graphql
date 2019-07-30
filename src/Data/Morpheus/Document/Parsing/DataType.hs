{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.Parsing.DataType
  ( parseDataType
  ) where

import           Data.Morpheus.Document.Parsing.Terms (Parser, nonNull, parseAssignment, parseMaybeTuple, pipe,
                                                       qualifier, token)
import           Data.Morpheus.Types.Internal.Data    (DataArgument, DataField (..), DataFingerprint (..),
                                                       DataFullType (..), DataOutputField, DataType (..),
                                                       DataTypeKind (..), DataTypeWrapper (..), Key)
import           Data.Text                            (Text)
import           Text.Megaparsec                      (between, label, many, sepBy1, sepEndBy, (<|>))
import           Text.Megaparsec.Char                 (char, space, space1, string)

wrapMock :: Parser ([DataTypeWrapper], Text)
wrapMock = do
  mock <- token
  space
  return ([], mock)

insideList :: Parser ([DataTypeWrapper], Text)
insideList =
  between
    (char '[' *> space)
    (char ']' *> space)
    (do (list, name) <- wrapMock <|> insideList
        nonNull' <- nonNull
        return ((ListType : nonNull') ++ list, name))

wrappedSignature :: Parser ([DataTypeWrapper], Text)
wrappedSignature = do
  sig <- insideList <|> wrapMock
  space
  return sig

dataArgument :: Parser (Text, DataArgument)
dataArgument =
  label "operatorArgument" $ do
    ((fieldName, _), (wrappers', fieldType)) <- parseAssignment qualifier wrappedSignature
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
entries = label "entries" $ between (char '{' *> space) (char '}' *> space) (entry `sepEndBy` many (char ',' *> space))
  where
    fieldWithArgs =
      label "fieldWithArgs" $ do
        (name, _) <- qualifier
        args <- parseMaybeTuple dataArgument
        return (name, args)
    entry =
      label "entry" $ do
        ((fieldName, fieldArgs), (wrappers', fieldType)) <- parseAssignment fieldWithArgs wrappedSignature
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

dataUnion :: Parser (Text, DataFullType)
dataUnion =
  label "dataUnion" $ do
    typeName <- typeDef "union"
    _ <- char '='
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
parseDataType = label "operator" $ dataObject <|> dataUnion
