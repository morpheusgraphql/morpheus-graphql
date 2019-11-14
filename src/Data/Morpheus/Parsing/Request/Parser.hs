{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Request.Parser
  ( parseGQL
  )
where

import qualified Data.Aeson                    as Aeson
                                                ( Value(..) )
import           Data.HashMap.Lazy              ( toList )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( ParseErrorBundle
                                                , eof
                                                , label
                                                , manyTill
                                                , runParser
                                                )

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser
                                                , processErrorBundle
                                                )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( spaceAndComments )
import           Data.Morpheus.Parsing.Request.Operation
                                                ( parseOperation )
import           Data.Morpheus.Parsing.Request.Selection
                                                ( parseFragmentDefinition )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value(..)
                                                , replaceValue
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest(..) )
import           Data.Morpheus.Types.Types      ( GQLQueryRoot(..) )

parseGQLSyntax :: Text -> Either (ParseErrorBundle Text Void) GQLQueryRoot
parseGQLSyntax = runParser request "<input>"
 where
  request :: Parser GQLQueryRoot
  request = label "GQLQueryRoot" $ do
    spaceAndComments
    operation <- parseOperation
    fragments <- manyTill parseFragmentDefinition eof
    pure GQLQueryRoot { operation, fragments, inputVariables = [] }

parseGQL :: GQLRequest -> Validation GQLQueryRoot
parseGQL GQLRequest { query, variables } = case parseGQLSyntax query of
  Right root       -> pure $ root { inputVariables = toVariableMap variables }
  Left  parseError -> failure $ processErrorBundle parseError
 where
  toVariableMap :: Maybe Aeson.Value -> [(Text, Value)]
  toVariableMap (Just (Aeson.Object x)) = map toMorpheusValue (toList x)
    where toMorpheusValue (key, value) = (key, replaceValue value)
  toVariableMap _ = []
