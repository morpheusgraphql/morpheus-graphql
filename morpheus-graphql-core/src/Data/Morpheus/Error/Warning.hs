{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Warning
  ( renderGQLErrors,
    deprecatedEnum,
    deprecatedField,
    gqlWarnings,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Morpheus.Types.Internal.AST.Base
  ( Description,
    Ref (..),
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( GQLError (locations, message),
    GQLErrors,
    at,
    msg,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( FieldName,
  )
import Language.Haskell.TH
import Relude

renderGQLErrors :: GQLErrors -> String
renderGQLErrors = unpack . encode . toList

-- TODO: implement warnings, is not used
deprecatedEnum :: FieldName -> Ref FieldName -> Maybe Description -> GQLError
deprecatedEnum typeName Ref {refPosition, refName} reason =
  "the enum value "
    <> msg typeName
    <> "."
    <> msg refName
    <> " is deprecated."
    <> msg (maybe "" (" " <>) reason)
    `at` refPosition

deprecatedField :: FieldName -> Ref FieldName -> Maybe Description -> GQLError
deprecatedField typeName Ref {refPosition, refName} reason =
  "the field "
    <> msg typeName
    <> "."
    <> msg refName
    <> " is deprecated."
    <> msg (maybe "" (" " <>) reason)
    `at` refPosition

gqlWarnings :: [GQLError] -> Q ()
gqlWarnings [] = pure ()
gqlWarnings warnings = traverse_ handleWarning warnings
  where
    handleWarning warning =
      reportWarning
        ( "\x1b[33m Morpheus GraphQL Warning: "
            <> toString (message warning)
            <> "\n"
            <> (unpack . encode) warning
            <> "\x1b[33m"
        )
