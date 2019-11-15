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
import           Data.Morpheus.Error.Utils      ( renderErrors
                                                , renderError
                                                , errorMessage
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( GQLErrors )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Ref(..)
                                                , Description
                                                , Name
                                                )
import           Language.Haskell.TH            ( Q
                                                , reportWarning
                                                )

renderGQLErrors :: GQLErrors -> String
renderGQLErrors = unpack . encode . renderErrors

deprecatedEnum :: Ref -> Description -> GQLErrors
deprecatedEnum Ref { refPosition, refName } reason =
  errorMessage refPosition
    $  "the enum value "
    <> refName
    <> "is deprecated. "
    <> reason

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
  handleWarning warning = reportWarning
    ("Morpheus Client Warning: " <> (unpack . encode . renderError) warning)
