{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Request.Parser (parseGQL) where

--
-- MORPHEUS
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
  ( Value (..),
  )
import Data.HashMap.Lazy (toList)
import Data.Morpheus.Internal.Utils
  ( fromElems,
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
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    GQLQuery (..),
    ResolvedValue,
    replaceValue,
    toLBS,
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
  ignoredTokens
  operation <- parseOperation
  fragments <- manyTill parseFragmentDefinition eof >>= lift . fromElems
  pure GQLQuery {operation, fragments, inputVariables = []}

parseGQL :: GQLRequest -> Eventless GQLQuery
parseGQL GQLRequest {query, variables} = setVariables <$> processParser request (toLBS query)
  where
    setVariables root = root {inputVariables = toVariableMap variables}
    toVariableMap :: Maybe Aeson.Value -> [(FieldName, ResolvedValue)]
    toVariableMap (Just (Aeson.Object x)) = map toMorpheusValue (toList x)
      where
        toMorpheusValue (key, value) = (FieldName key, replaceValue value)
    toVariableMap _ = []
