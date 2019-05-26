{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Internal
  ( GQLSyntax(..)
  , syntaxFail
  , catchError
  ) where

import qualified Data.Attoparsec.Internal.Types as AT
import           Data.Attoparsec.Text           (Parser)
import           Data.Text                      (Text, pack, unpack)

data GQLSyntax a
  = Invalid Text
            Int
  | Valid a

syntaxFail :: Text -> Parser a
syntaxFail err = AT.Parser parser
  where
    parser t pos more lose _success = lose t pos more [] msg
    msg = "Syntax Error: " ++ unpack err

catchError :: Parser a -> Parser (GQLSyntax a)
catchError parser = transform (Valid <$> parser)
  where
    transform :: AT.Parser i (GQLSyntax a) -> AT.Parser i (GQLSyntax a)
    transform f =
      AT.Parser $ \t pos more lose success ->
        let lose' t' position' more' _ msg' = AT.runParser (astError msg' position') t' pos more' lose success
         in AT.runParser f t pos more lose' success
    astError message position' = return (Invalid (pack message) (AT.fromPos position'))
