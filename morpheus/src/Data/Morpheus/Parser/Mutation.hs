{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Mutation (mutation) where

import      Data.Text                      (Text)
import      Data.Morpheus.Parser.RootHead  (rootHeadArguments)
import      Data.Morpheus.Parser.Primitive ( token)
import      Data.Attoparsec.Text      (Parser, try, string, skipSpace )
import      Data.Morpheus.Types.Types     ( QuerySelection(..), Arguments(..), GQLOperator(..))
import      Control.Applicative            ((<|>))
import      Data.Morpheus.Parser.Body     ( body )


mutation :: Parser GQLOperator
mutation = do
    string "mutation "
    skipSpace
    name <- token
    variables <- try (skipSpace *> rootHeadArguments) <|> pure []
    MutationOperator name <$> body variables