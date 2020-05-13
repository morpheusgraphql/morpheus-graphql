{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Request.Parser (parseGQL) where

import qualified Data.Aeson as Aeson
  ( Value (..),
  )
import Data.HashMap.Lazy (toList)
--
-- MORPHEUS
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    processParser,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( spaceAndComments,
  )
import Data.Morpheus.Parsing.Request.Operation
  ( parseOperation,
  )
import Data.Morpheus.Parsing.Request.Selection
  ( parseFragmentDefinition,
  )
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST
  ( GQLQuery (..),
    Name,
    ResolvedValue,
    replaceValue,
  )
import Data.Morpheus.Types.Internal.Operation
  ( fromList,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Text.Megaparsec
  ( eof,
    label,
    manyTill,
  )

request :: Parser GQLQuery
request = label "GQLQuery" $ do
  spaceAndComments
  operation <- parseOperation
  fragments <- manyTill parseFragmentDefinition eof >>= fromList
  pure GQLQuery {operation, fragments, inputVariables = []}

parseGQL :: GQLRequest -> Eventless GQLQuery
parseGQL GQLRequest {query, variables} = setVariables <$> processParser request query
  where
    setVariables root = root {inputVariables = toVariableMap variables}
    toVariableMap :: Maybe Aeson.Value -> [(Name, ResolvedValue)]
    toVariableMap (Just (Aeson.Object x)) = map toMorpheusValue (toList x)
      where
        toMorpheusValue (key, value) = (key, replaceValue value)
    toVariableMap _ = []
