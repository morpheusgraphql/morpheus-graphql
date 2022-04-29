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
import Language.Haskell.TH
import Relude
import Spec.Utils
  ( path,
  )

loc :: FilePath -> FilePath
loc x = path ("LocalGlobal/" <> x)

schema :: FilePath
schema = loc "schema.gql"

declareAPITypes :: Maybe FilePath -> Q [Dec]
declareAPITypes = declareClientTypes schema . fmap loc

declareAPITypesInline :: Text -> Q [Dec]
declareAPITypesInline = declareClientTypesInline schema
