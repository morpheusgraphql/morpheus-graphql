{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Warning
  ( renderGQLErrors,
    deprecatedEnum,
    deprecatedField,
    gqlWarnings,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable (traverse_)
-- MORPHEUS
import Data.Morpheus.Error.Utils (errorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( Description,
    GQLErrors,
    Name,
    Ref (..),
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH
  ( Q,
    reportWarning,
  )

renderGQLErrors :: GQLErrors -> String
renderGQLErrors = unpack . encode

deprecatedEnum :: Name -> Ref -> Maybe Description -> GQLErrors
deprecatedEnum typeName Ref {refPosition, refName} reason =
  errorMessage refPosition $
    "the enum value "
      <> typeName
      <> "."
      <> refName
      <> "\" is deprecated."
      <> maybe "" (" " <>) reason

deprecatedField :: Name -> Ref -> Maybe Description -> GQLErrors
deprecatedField typeName Ref {refPosition, refName} reason =
  errorMessage refPosition $
    "the field \""
      <> typeName
      <> "."
      <> refName
      <> "\" is deprecated."
      <> maybe "" (" " <>) reason

gqlWarnings :: GQLErrors -> Q ()
gqlWarnings [] = pure ()
gqlWarnings warnings = traverse_ handleWarning warnings
  where
    handleWarning warning =
      reportWarning ("Morpheus GraphQL Warning: " <> (unpack . encode) warning)
