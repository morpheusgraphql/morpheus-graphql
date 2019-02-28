{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Mutation (mutation) where

import      Data.Text                      (Text)
import      Data.Morpheus.Parser.RootHead  (rootHeadArguments)
import      Data.Morpheus.Parser.Primitive ( token)
import      Data.Attoparsec.Text      (Parser, try, string, skipSpace )
import      Data.Morpheus.Types.Types     ( Arguments)
import           Control.Applicative            ((<|>))


mutation :: Parser  (Text,Arguments)
mutation = do
    string "mutation "
    skipSpace
    queryName <- token
    variables <- try (skipSpace *> rootHeadArguments) <|> pure []
    pure (queryName, variables)