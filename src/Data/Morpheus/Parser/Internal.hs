module Data.Morpheus.Parser.Internal
  ( GQLSyntax(..)
  ) where

import           Data.Text (Text)

data GQLSyntax a
  = Invalid Int
            Text
  | Valid a
