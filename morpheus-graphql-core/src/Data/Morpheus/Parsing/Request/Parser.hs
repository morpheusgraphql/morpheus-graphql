{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Request.Parser (parseGQL) where

--
-- MORPHEUS
import qualified Data.Aeson as Aeson
  ( Value (..),
  )
import Data.HashMap.Lazy (toList)
import Data.Morpheus.Internal.Utils
  ( fromElems,
    toLBS,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    processParser,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( ignoredTokens,
  )
import Data.Morpheus.Parsing.Request.Operation
  ( parseOperation,
  )
import Data.Morpheus.Parsing.Request.Selection
  ( parseFragmentDefinition,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    GQLQuery (..),
    ResolvedValue,
    replaceValue,
  )
import Data.Morpheus.Types.Internal.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Eventless,
  )
import Relude hiding
  ( many,
    toList,
  )
import Text.Megaparsec
  ( eof,
    label,
    many,
  )

request :: Parser GQLQuery
request =
  label "GQLQuery" $
    ( GQLQuery []
        <$> (ignoredTokens *> parseOperation)
        <*> (many parseFragmentDefinition >>= lift . fromElems)
    )
      <* ignoredTokens
      <* eof

parseGQL :: GQLRequest -> Eventless GQLQuery
parseGQL GQLRequest {query, variables} =
  setVariables
    <$> processParser request (toLBS query)
  where
    setVariables root = root {inputVariables = toVariableMap variables}
    toVariableMap :: Maybe Aeson.Value -> [(FieldName, ResolvedValue)]
    toVariableMap (Just (Aeson.Object x)) = map toMorpheusValue (toList x)
      where
        toMorpheusValue (key, value) = (FieldName key, replaceValue value)
    toVariableMap _ = []
