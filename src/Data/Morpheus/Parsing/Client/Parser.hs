{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Client.Parser
  ( lookupGQLConfig
  ) where

import           Data.Text                               (Text)
import           Text.Megaparsec                         (label, runParser)
import           Text.Megaparsec.Char                    (space)

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Client.ParseMeta  (parseMeta)
import           Data.Morpheus.Parsing.Internal.Internal (Parser, processErrorBundle)
import           Data.Morpheus.Types.Internal.Validation (Validation)

lookupGQLConfig :: Text -> Validation (Text, Text)
lookupGQLConfig text =
  case runParser request "<input>" text of
    Left x  -> Left $ processErrorBundle x
    Right x -> Right x
  where
    request :: Parser (Text, Text)
    request =
      label "GQLConfig" $ do
        space
        parseMeta
