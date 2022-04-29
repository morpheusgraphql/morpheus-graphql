{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.LocalGlobal.Api
  ( declareAPITypes,
    declareAPITypesInline,
  )
where

import Data.Maybe
import Data.Morpheus.Client
  ( declareClientTypes,
    declareClientTypesInline,
  )
import Data.Morpheus.Types.Internal.AST (FieldName)
import Data.Text
import Language.Haskell.TH
import Relude
import Spec.Utils
  ( relativePath,
  )

schema :: Q FilePath
schema = relativePath "LocalGlobal/schema.gql"

declareAPITypes :: Maybe FieldName -> Q [Dec]
declareAPITypes src = declareClientTypes schema (relativePath <$> src)

declareAPITypesInline :: Text -> Q [Dec]
declareAPITypesInline = declareClientTypesInline schema
