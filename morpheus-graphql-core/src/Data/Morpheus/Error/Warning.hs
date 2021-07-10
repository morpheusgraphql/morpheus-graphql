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
import Data.Morpheus.Error.Utils (errorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( Description,
    GQLErrors,
    Ref (..),
    msg,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( FieldName,
  )
import Language.Haskell.TH (Q, reportWarning)
import Relude

renderGQLErrors :: GQLErrors -> String
renderGQLErrors = unpack . encode

deprecatedEnum :: FieldName -> Ref FieldName -> Maybe Description -> GQLErrors
deprecatedEnum typeName Ref {refPosition, refName} reason =
  errorMessage refPosition $
    "the enum value "
      <> msg typeName
      <> "."
      <> msg refName
      <> " is deprecated."
      <> msg (maybe "" (" " <>) reason)

deprecatedField :: FieldName -> Ref FieldName -> Maybe Description -> GQLErrors
deprecatedField typeName Ref {refPosition, refName} reason =
  errorMessage refPosition $
    "the field "
      <> msg typeName
      <> "."
      <> msg refName
      <> " is deprecated."
      <> msg (maybe "" (" " <>) reason)

gqlWarnings :: GQLErrors -> Q ()
gqlWarnings [] = pure ()
gqlWarnings warnings = traverse_ handleWarning warnings
  where
    handleWarning warning =
      reportWarning ("Morpheus GraphQL Warning: " <> (unpack . encode) warning)
