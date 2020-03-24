{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Request.Parser
  ( parseGQL
  , processParser
  )
where

import qualified Data.Aeson                    as Aeson
                                                ( Value(..) )
import           Data.HashMap.Lazy              ( toList )
import           Data.Text                      ( Text )
import           Text.Megaparsec                ( eof
                                                , label
                                                , manyTill
                                                )

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser
                                                , processParser
                                                )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( spaceAndComments )
import           Data.Morpheus.Parsing.Request.Operation
                                                ( parseOperation )
import           Data.Morpheus.Parsing.Request.Selection
                                                ( parseFragmentDefinition )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation )
import           Data.Morpheus.Types.Internal.AST
                                                ( replaceValue
                                                , GQLQuery(..)
                                                , ResolvedValue
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest(..) )

parseGQLSyntax :: Text -> Validation GQLQuery
parseGQLSyntax = processParser request
 where
  request :: Parser GQLQuery
  request = label "GQLQuery" $ do
    spaceAndComments
    operation <- parseOperation
    fragments <- manyTill parseFragmentDefinition eof
    pure GQLQuery { operation, fragments, inputVariables = [] }

parseGQL :: GQLRequest -> Validation GQLQuery
parseGQL GQLRequest { query, variables } = setVariables <$> parseGQLSyntax query 
 where
  setVariables root = root { inputVariables = toVariableMap variables }
  toVariableMap :: Maybe Aeson.Value -> [(Text, ResolvedValue)]
  toVariableMap (Just (Aeson.Object x)) = map toMorpheusValue (toList x)
    where toMorpheusValue (key, value) = (key, replaceValue value)
  toVariableMap _ = []