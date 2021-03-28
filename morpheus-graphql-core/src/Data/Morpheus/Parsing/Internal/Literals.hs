{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Literals
  ( at,
    colon,
    equal,
    ignoredTokens,
    ignoredTokens1,
    symbol,
    pipe,
  )
where

import Data.Morpheus.Parsing.Internal.Internal (Parser)
import Relude hiding (ByteString, empty, many)
import Text.Megaparsec
  ( label,
    many,
    sepBy1,
    skipManyTill,
  )
import Text.Megaparsec.Byte
  ( char,
    newline,
    printChar,
    space,
    space1,
  )

-- ','
#define COMMA 44
-- '@'
#define AT 64
-- '='
#define EQUAL 61
-- ':'
#define COLON 58
-- '|'
#define PIPE 124
-- '#'
#define HASH_TAG 35

at :: Parser ()
at = symbol AT
{-# INLINEABLE at #-}

equal :: Parser ()
equal = symbol EQUAL
{-# INLINEABLE equal #-}

colon :: Parser ()
colon = symbol COLON
{-# INLINEABLE colon #-}

symbol :: Word8 -> Parser ()
symbol x = char x *> ignoredTokens
{-# INLINEABLE symbol #-}

-- Ignored Tokens : https://graphql.github.io/graphql-spec/June2018/#sec-Source-Text.Ignored-Tokens
--  Ignored:
--    UnicodeBOM
--    WhiteSpace
--    LineTerminator
--    Comment
--    Comma
ignoredTokens :: Parser ()
ignoredTokens =
  label "IgnoredTokens" $
    space
      *> many (comment <|> (char COMMA *> space))
      *> space
  where
    comment =
      label "Comment" $
        char HASH_TAG *> skipManyTill printChar newline *> space
    {-# INLINEABLE comment #-}
{-# INLINEABLE ignoredTokens #-}

ignoredTokens1 :: Parser ()
ignoredTokens1 = space1 *> ignoredTokens
{-# INLINEABLE ignoredTokens1 #-}

pipe :: Parser a -> Parser [a]
pipe x = optional (symbol PIPE) *> (x `sepBy1` symbol PIPE)
{-# INLINEABLE pipe #-}
