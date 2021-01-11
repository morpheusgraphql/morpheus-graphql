{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Request.Parser
  ( parseRequest,
  )
where

--
-- MORPHEUS
import qualified Data.Aeson as Aeson
  ( Value (..),
  )
import Data.HashMap.Lazy (toList)
import Data.Morpheus.Ext.Result
  ( Eventless,
  )
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
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST
  ( ExecutableDocument (..),
    FieldName (..),
    ResolvedValue,
    replaceValue,
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

parseExecutableDocument :: [(FieldName, ResolvedValue)] -> Parser ExecutableDocument
parseExecutableDocument variables =
  label "ExecutableDocument" $
    ( ExecutableDocument variables
        <$> (ignoredTokens *> parseOperation)
        <*> (many parseFragmentDefinition >>= lift . fromElems)
    )
      <* ignoredTokens
      <* eof

parseRequest :: GQLRequest -> Eventless ExecutableDocument
parseRequest GQLRequest {query, variables} =
  processParser
    (parseExecutableDocument $ toVariableMap variables)
    (toLBS query)
  where
    toVariableMap :: Maybe Aeson.Value -> [(FieldName, ResolvedValue)]
    toVariableMap (Just (Aeson.Object x)) = map toMorpheusValue (toList x)
      where
        toMorpheusValue (key, value) = (FieldName key, replaceValue value)
    toVariableMap _ = []
