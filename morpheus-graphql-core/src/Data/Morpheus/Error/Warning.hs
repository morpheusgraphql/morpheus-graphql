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
    printError,
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
printWarning = printError "warning" "\x1b[33m"

printError :: String -> String -> GQLError -> String
printError label color warning =
  withColor (label <> ":")
    <> description
    <> loc
    <> printedPath
    <> "\n"
  where
    propPath = concat $ toList $ path warning
    propLoc = concat $ toList $ locations warning
    description = indent <> withColor (toString (message warning))
    loc
      | null propPath = ""
      | otherwise = indent <> "  locations: " <> concatMap printLocation propLoc
    printedPath
      | null propPath = ""
      | otherwise = indent <> "  path: " <> intercalate "." (map printPath propPath)

    withColor :: String -> String
    withColor x = color <> x <> "\x1b[0m"

indent :: String
indent = "\n      "

printPath :: PropName -> String
printPath (PropIndex x) = show x
printPath (PropName x) = T.unpack x

printLocation :: Position -> String
printLocation Position {..} = show line <> ":" <> show column
