{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Client.Client
  ( renderGQLErrors
  , deprecatedEnum
  , deprecatedField
  , gqlWarnings
  )
where

import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Ref(..)
                                                , Description
                                                , Name
                                                , GQLErrors
                                                )
import           Language.Haskell.TH            ( Q
                                                , reportWarning
                                                )
import           Data.Semigroup                 ( (<>) )

renderGQLErrors :: GQLErrors -> String
renderGQLErrors = unpack . encode

deprecatedEnum :: Name -> Ref -> Maybe Description -> GQLErrors
deprecatedEnum typeName Ref { refPosition, refName } reason =
  errorMessage refPosition
    $  "the enum value "
    <> typeName
    <> "."
    <> refName
    <> "\" is deprecated."
    <> maybe "" (" " <>) reason

deprecatedField :: Name -> Ref -> Maybe Description -> GQLErrors
deprecatedField typeName Ref { refPosition, refName } reason =
  errorMessage refPosition
    $  "the field \""
    <> typeName
    <> "."
    <> refName
    <> "\" is deprecated."
    <> maybe "" (" " <>) reason

gqlWarnings :: GQLErrors -> Q ()
gqlWarnings []       = pure ()
gqlWarnings warnings = mapM_ handleWarning warnings
 where
  handleWarning warning =
    reportWarning ("Morpheus GraphQL Warning: " <> (unpack . encode) warning)
