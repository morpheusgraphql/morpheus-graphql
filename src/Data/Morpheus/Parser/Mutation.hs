{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Mutation (mutation) where

import      Data.Text                      (Text)
import      Data.Morpheus.Parser.RootHead  (rootHeadArguments)
import      Data.Morpheus.Parser.Primitive ( token)
import      Data.Attoparsec.Text      (Parser, try, string, skipSpace )
import      Data.Morpheus.Types.Types     ( QuerySelection(..), Arguments(..))
import      Control.Applicative            ((<|>))
import      Data.Morpheus.Parser.Body     ( body )


mutation :: Parser QuerySelection
mutation = do
    string "mutation "
    skipSpace
    queryName <- token
    variables <- try (skipSpace *> rootHeadArguments) <|> pure []
    body variables