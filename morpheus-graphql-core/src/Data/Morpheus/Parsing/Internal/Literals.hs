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
    satisfy,
    sepBy1,
    skipManyTill,
    takeWhile1P,
    takeWhileP,
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

-- White Space
--
-- Horizontal: Tab (U+0009) https://codepoints.net/U+0009
#define TABULATION 9
-- Space (U+0020) https://codepoints.net/U+0020
#define SPACE 32
-- UnicodeBOM (U+FEFF): https://codepoints.net/U+FEFF
#define UNICODE_BOM 65279
-- New Line (U+000A)
#define NEW_LINE 10
-- Carriage Return (U+000D) https://codepoints.net/U+000D
#define CARRIAGE_RETURN 13
-- U+FFFF : https://codepoints.net/U+FFFF
#define NON_CHARACTER 65535

at :: Parser ()
at = symbol AT
{-# INLINE at #-}

equal :: Parser ()
equal = symbol EQUAL
{-# INLINE equal #-}

colon :: Parser ()
colon = symbol COLON
{-# INLINE colon #-}

symbol :: Word8 -> Parser ()
symbol x = char x *> ignoredTokens
{-# INLINE symbol #-}

-- Ignored Tokens : https://graphql.github.io/graphql-spec/June2018/#sec-Source-Text.Ignored-Tokens
ignoredTokens :: Parser ()
ignoredTokens = label "IgnoredTokens" $ many ignored $> ()
{-# INLINE ignoredTokens #-}

-- isIgnored :: UnicodeBOM, WhiteSpace, LineTerminator, Comment , Comma
ignored :: Parser ()
ignored = (takeWhile1P Nothing isIgnored <|> comment) $> ()
  where
    isIgnored x =
      (x >= TABULATION && x <= CARRIAGE_RETURN)
        || x == SPACE
        || x == COMMA
        || x == UNICODE_BOM
    comment = char HASH_TAG *> takeWhileP Nothing (/= NEW_LINE)
    {-# INLINE comment #-}
{-# INLINE ignored #-}

ignoredTokens1 :: Parser ()
ignoredTokens1 = space1 *> ignoredTokens
{-# INLINE ignoredTokens1 #-}

pipe :: Parser a -> Parser [a]
pipe x = optional (symbol PIPE) *> (x `sepBy1` symbol PIPE)
{-# INLINE pipe #-}
