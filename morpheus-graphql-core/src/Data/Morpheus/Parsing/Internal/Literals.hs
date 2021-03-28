{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Literals
  ( Lit (..),
    ampersand,
    at,
    colon,
    comma,
    dollar,
    equal,
    exclamationMark,
    ignoredTokens,
    ignoredTokens1,
    minus,
    symbol,
    verticalPipe,
    underscore,
  )
where

import Data.Morpheus.Parsing.Internal.Internal (Parser)
import GHC.TypeLits (Symbol)
import Relude hiding (ByteString, empty, many)
import Text.Megaparsec
  ( label,
    many,
    skipManyTill,
  )
import Text.Megaparsec.Byte
  ( char,
    newline,
    printChar,
    space,
    space1,
  )

class Lit (l :: Symbol) where
  lit :: f l -> Word8

instance Lit "," where
  lit _ = 44
  {-# INLINEABLE lit #-}

instance Lit "_" where
  lit _ = 95
  {-# INLINEABLE lit #-}

instance Lit "!" where
  lit _ = 33
  {-# INLINEABLE lit #-}

instance Lit "$" where
  lit _ = 36
  {-# INLINEABLE lit #-}

instance Lit "@" where
  lit _ = 64
  {-# INLINEABLE lit #-}

-- dollar :: $
dollar :: Parser ()
dollar = symbol $ lit $ Proxy @"$"
{-# INLINEABLE dollar #-}

-- at: '@'
at :: Parser ()
at = symbol $ lit $ Proxy @"@"
{-# INLINEABLE at #-}

-- equal :: '='
-- label "="
equal :: Parser ()
equal = symbol 61
{-# INLINEABLE equal #-}

-- colon :: ':'
-- label ":"
colon :: Parser ()
colon = symbol 58
{-# INLINEABLE colon #-}

-- minus: '-'
-- label "-"
minus :: Parser ()
minus = symbol 45
{-# INLINEABLE minus #-}

-- exclamationMark: '!'
exclamationMark :: Parser ()
exclamationMark = symbol $ lit $ Proxy @"!"
{-# INLINEABLE exclamationMark #-}

-- verticalPipe: '|'
-- label "|"
verticalPipe :: Parser ()
verticalPipe = symbol 124
{-# INLINEABLE verticalPipe #-}

-- label "&"
ampersand :: Parser ()
ampersand = symbol 38
{-# INLINEABLE ampersand #-}

symbol :: Word8 -> Parser ()
symbol x = char x *> ignoredTokens
{-# INLINEABLE symbol #-}

-- comma: ,
comma :: Parser ()
comma = char (lit (Proxy @",")) *> space
{-# INLINEABLE comma #-}

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
      *> many (comment <|> comma)
      *> space
  where
    comment =
      label "Comment" $
        char 35 *> skipManyTill printChar newline *> space
    {-# INLINEABLE comment #-}
{-# INLINEABLE ignoredTokens #-}

ignoredTokens1 :: Parser ()
ignoredTokens1 = space1 *> ignoredTokens
{-# INLINEABLE ignoredTokens1 #-}

-- underscore : '_'
underscore :: Parser Word8
underscore = char (lit (Proxy @"_"))
{-# INLINEABLE underscore #-}
