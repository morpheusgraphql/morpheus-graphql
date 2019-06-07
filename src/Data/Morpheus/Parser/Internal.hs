module Data.Morpheus.Parser.Internal
  ( GQLSyntax(..)
  , Parser
  , Position
  ) where

import           Data.Text       (Text)
import           Data.Void       (Void)
import           Text.Megaparsec (Parsec, SourcePos)

type Position = SourcePos

type Parser = Parsec Void Text

data GQLSyntax a
  = Invalid Text
            Position
  | Valid a
