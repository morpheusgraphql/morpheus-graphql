{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch.GQLClient
  ( GQLClient (..),
    withHeaders,
    Headers,
    Header,
  )
where

import Relude hiding (ByteString)

type Headers = Map Text Text

type Header = (Text, Text)

data GQLClient = GQLClient
  { clientHeaders :: Headers,
    clientURI :: String
  }

instance IsString GQLClient where
  fromString clientURI =
    GQLClient
      { clientURI,
        clientHeaders = fromList [("Content-Type", "application/json")]
      }

withHeaders :: GQLClient -> [Header] -> GQLClient
withHeaders GQLClient {..} headers = GQLClient {clientHeaders = clientHeaders <> fromList headers, ..}
