{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Warning
  ( renderGQLErrors,
    deprecatedEnum,
    deprecatedField,
    gqlWarnings,
    printWarning,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Morpheus.Types.Internal.AST.Base
  ( Description,
    Position (..),
    Ref (..),
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( GQLError (..),
    GQLErrors,
    PropName (..),
    at,
    msg,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( FieldName,
  )
import qualified Data.Text as T
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
gqlWarnings warnings = traverse_ (reportWarning . printWarning) warnings

printWarning :: GQLError -> String
printWarning warning =
  yellow "warning:"
    <> description
    <> loc
    <> p
    <> "\n"
  where
    yellow x = "\x1b[33m" <> x <> "\x1b[0m"
    description = indent <> yellow (toString (message warning))
    loc = indent <> "  loc: " <> concatMap printLocation (concat $ toList $ locations warning)
    p = indent <> "  path: " <> intercalate "/" (map printPath $ concat $ toList $ path warning)
    indent = "\n      "

printPath :: PropName -> String
printPath (PropIndex x) = show x
printPath (PropName x) = T.unpack x

printLocation :: Position -> String
printLocation Position {..} = show line <> ":" <> show column