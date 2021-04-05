{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Data.Morpheus.Parsing.Internal.String
  ( parseStringBS,
  )
where

import Data.ByteString.Lazy.Internal
  ( ByteString,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
  )
import Data.Morpheus.Parsing.Internal.Literals
  ( ignoredTokens,
  )
import Relude hiding (ByteString, empty, many)
import Text.Megaparsec
  ( choice,
    label,
    satisfy,
    takeWhileP,
    unexpected,
  )
import Text.Megaparsec.Byte
  ( char,
    string,
  )
import Text.Megaparsec.Error
  ( ErrorItem (..),
  )

#define DOUBLE_QUOTE 34

#define CHAR_b 98

#define CHAR_f 102

#define CHAR_n 110

#define CHAR_r 114

#define CHAR_t 116

#define BACKSLASH 92

#define NEW_LINE 10

-- https://spec.graphql.org/June2018/#sec-Source-Text
-- SourceCharacter : [\u0009\u000A\u000D\u0020-\uFFFF]/
isSourceCharacter :: Word8 -> Bool
isSourceCharacter 0x0009 = True
isSourceCharacter 0x000A = True
isSourceCharacter 0x000D = True
isSourceCharacter x = 0x0020 <= x && x <= 0xFFFF
{-# INLINE isSourceCharacter #-}

inlineString :: Parser ByteString
inlineString =
  label "String" $
    char DOUBLE_QUOTE
      *> parseContent
      <* ignoredTokens
{-# INLINE inlineString #-}

parseContent :: Parser ByteString
parseContent = do
  xs <- takeWhileP Nothing (\x -> isSourceCharacter x && DOUBLE_QUOTE /= x && x /= BACKSLASH && NEW_LINE /= x)
  z <- satisfy (const True)
  case z of
    DOUBLE_QUOTE -> pure xs
    BACKSLASH -> (xs <>) <$> ((<>) <$> escapeChar <*> parseContent)
    w -> unexpected (Tokens (w :| []))
  where
    escapeChar :: Parser ByteString
    escapeChar =
      choice
        [ char CHAR_b $> "\b",
          char CHAR_f $> "\f",
          char CHAR_n $> "\n",
          char CHAR_r $> "\r",
          char CHAR_t $> "\t",
          char BACKSLASH $> "\\",
          char DOUBLE_QUOTE $> "\"",
          char 47 $> "/"
        ]
    {-# INLINE escapeChar #-}
{-# INLINE parseContent #-}

blockString :: Parser ByteString
blockString = string "\"\"\"" *> content <* ignoredTokens
  where
    content :: Parser ByteString
    content = do
      text <- takeWhileP Nothing (\x -> isSourceCharacter x && x /= DOUBLE_QUOTE)
      doubleQuotes <- takeWhileP Nothing (== DOUBLE_QUOTE)
      case doubleQuotes of
        "\"\"\"" -> pure text
        _ -> ((text <> doubleQuotes) <>) <$> content
    {-# INLINE content #-}
{-# INLINE blockString #-}

parseStringBS :: Parser ByteString
parseStringBS = blockString <|> inlineString
{-# INLINE parseStringBS #-}
