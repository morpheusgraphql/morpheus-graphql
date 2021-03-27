{-# LANGUAGE OverloadedStrings #-}

module Parser.Morpheus
  ( parseText,
    parseByteString,
    countParsedTypes,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Core (parseTypeDefinitions)
import Data.Morpheus.Internal.Ext (resultOr)
import Data.Text (Text)

countParsedTypes :: ByteString -> Int
countParsedTypes bs = resultOr (const 0) length (parseTypeDefinitions bs)

parseByteString :: ByteString -> ByteString
parseByteString x = resultOr (error . show) (const "OK") (parseTypeDefinitions x)

parseText :: Text -> Text
parseText x = resultOr (error . show) (const "OK") (parseTypeDefinitions x)
