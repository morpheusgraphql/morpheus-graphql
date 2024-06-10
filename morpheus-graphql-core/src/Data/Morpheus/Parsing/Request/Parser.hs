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
import Data.Morpheus.Ext.Result
  ( GQLResult,
  )
import Data.Morpheus.Internal.Utils
  ( IsMap (toAssoc, unsafeFromList),
    empty,
    fromElems,
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
    Variables,
    packName,
    replaceValue,
  )
import Relude hiding
  ( empty,
    fromList,
    many,
    toList,
  )
import Text.Megaparsec
  ( eof,
    label,
    many,
  )

parseExecutableDocument :: Variables -> Parser ExecutableDocument
parseExecutableDocument variables =
  label "ExecutableDocument"
    $ ( ExecutableDocument variables
          <$> (ignoredTokens *> parseOperation)
          <*> (many parseFragmentDefinition >>= lift . fromElems)
      )
    <* ignoredTokens
    <* eof

parseRequest :: GQLRequest -> GQLResult ExecutableDocument
parseRequest GQLRequest {query, variables} =
  processParser
    (parseExecutableDocument $ toVariables variables)
    (toLBS query)
  where
    toVariables :: Maybe Aeson.Value -> Variables
    toVariables (Just (Aeson.Object x)) = unsafeFromList $ toMorpheusValue <$> toAssoc x
      where
        toMorpheusValue (key, value) = (packName key, replaceValue value)
    toVariables _ = empty
