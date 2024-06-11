{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.QuasiQuoter
  ( raw,
  )
where

import qualified Data.Text as T
import Language.Haskell.TH.Quote
import Relude hiding (ByteString)

notSupported :: Text -> a
notSupported things =
  error
    $ things
    <> " are not supported by the GraphQL QuasiQuoter"

-- | QuasiQuoter to insert multiple lines of text in Haskell
raw :: QuasiQuoter
raw =
  QuasiQuoter
    { quoteExp = \txt -> [|T.pack txt|],
      quotePat = notSupported "Patterns",
      quoteType = notSupported "Types",
      quoteDec = notSupported "Declarations"
    }
