{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Client.Client
  ( renderGQLErrors
  , deprecatedEnum
  , deprecatedField
  )
where

import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Data.Morpheus.Error.Utils      ( renderErrors
                                                , errorMessage
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( GQLErrors )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Ref(..)
                                                , Description
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

deprecatedField :: Ref -> Description -> GQLErrors
deprecatedField Ref { refPosition, refName } reason =
  errorMessage refPosition
    $  "the field "
    <> refName
    <> "is deprecated. "
    <> reason
