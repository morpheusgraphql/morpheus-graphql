{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.LocalGlobal.Api
  ( declareAPITypes,
  )
where

import Data.Maybe
import Data.Morpheus.Client (declareClientTypesInline)
import Data.Text
import Language.Haskell.TH
import Spec.Utils
  ( relativePath,
  )

declareAPITypes :: Maybe Text -> Q [Dec]
declareAPITypes = declareClientTypesInline (relativePath "LocalGlobal/schema.gql")
