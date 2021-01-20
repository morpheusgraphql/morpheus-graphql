{-# LANGUAGE FlexibleContexts #-}
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
import Data.Morpheus.Ext.SafeHashMap (unsafeFromList)
import Data.Morpheus.Internal.Utils
  ( empty,
    fromElems,
    toLBS,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    processParser,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( Term,
    ignoredTokens,
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
    Variables,
    replaceValue,
  )
import Relude hiding
  ( empty,
    fromList,
    many,
    toList,
  )
import Text.Megaparsec
  ( Stream,
    Tokens,
    eof,
    label,
    many,
  )

parseExecutableDocument :: (Stream s, Term s, IsString s, IsString (Tokens s)) => Variables -> Parser s ExecutableDocument
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
    (parseExecutableDocument $ toVariables variables)
    (toLBS query)
  where
    toVariables :: Maybe Aeson.Value -> Variables
    toVariables (Just (Aeson.Object x)) = unsafeFromList $ toMorpheusValue <$> toList x
      where
        toMorpheusValue (key, value) = (FieldName key, replaceValue value)
    toVariables _ = empty
